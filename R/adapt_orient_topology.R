#' Reorients the edges of the milestone network to the cell's RNA velocity vectors
#'
#' @inheritParams common_param
#' @inheritParams add_expression
#'
#' @return The trajectory with oriented *milestone_network* and *progressions*
#'
#' @examples
#' # we'll create a simple linear trajectory
#' cell_ids <- c("a", "b", "c", "d", "e")
#' pseudotime <- setNames(seq_along(cell_ids), cell_ids)
#' expression <- as.matrix(data.frame(
#'   a = pseudotime,
#'   b = pseudotime ** 2,
#'   c = log(pseudotime)
#' ))
#' expression_projected <- as.matrix(data.frame(
#'   a = (pseudotime + 1),
#'   b = (pseudotime + 1) ** 2,
#'   c = log(pseudotime + 1)
#' ))
#'
#' # the milestone network is "wrong" in the sense that B and A are oriented in the opposite direction
#' milestone_network <- tibble::tribble(
#'   ~from, ~to, ~length, ~directed,
#'   "B", "A", 1, TRUE,
#'   "B", "C", 1, TRUE
#' )
#' progressions <- tibble::tribble(
#'   ~cell_id, ~from, ~to, ~percentage,
#'   "a", "B", "A", 1,
#'   "b", "B", "A", 0.5,
#'   "c", "B", "A", 0,
#'   "d", "B", "C", 0.5,
#'   "e", "B", "C", 1
#' )
#'
#' trajectory <- wrap_expression(
#'   counts = expression,
#'   expression = expression,
#'   expression_projected = expression_projected
#' )
#' trajectory <- add_trajectory(
#'   trajectory,
#'   milestone_network = milestone_network,
#'   progressions = progressions
#' )
#'
#' trajectory_oriented <- orient_topology_to_velocity(trajectory)
#'
#' # the edge is now correctly oriented
#' trajectory_oriented$milestone_network
#' assertthat::assert_that(
#'   all(
#'     trajectory_oriented$milestone_network[2, c("from", "to")] == c("B", "C")
#'   )
#' )
#'
#' @export
orient_topology_to_velocity <- function(
  trajectory,
  expression = trajectory$expression,
  expression_projected = trajectory$expression_projected
) {
  # dummy proofing
  assert_that(is(trajectory, "dynwrap::with_trajectory"))
  assert_that(!is.null(expression))
  assert_that(!is.null(expression_projected))

  if (nrow(trajectory$divergence_regions)) {
    stop("Orienting topologies with divergence regions doesn't work yet")
  }

  flip_fractions <- pmap_dbl(trajectory$milestone_network, function(from, to, ...) {
    # order cells based on their percentage
    progressions_edge <- trajectory$progressions %>%
      filter(from == !!from, to == !!to) %>%
      arrange(desc(percentage))

    # find for each cell its nearest neighbor (not self) in the expression_projected
    nn_ix <- FNN::knnx.index(
      expression[progressions_edge$cell_id, ],
      expression_projected[progressions_edge$cell_id, ],
      k = 2
    )
    nn_ix[, 1][nn_ix[, 1] == seq_len(nrow(nn_ix))] <- NA
    nn_ix <- nn_ix %>% apply(1, function(x) first(x[!is.na(x)]))

    # find out whether RNA velocity support and edge reversal
    sum(nn_ix > seq_along(nn_ix))/sum(nn_ix < seq_along(nn_ix))
  })

  milestone_network_toflip <- trajectory$milestone_network %>%
    filter(flip_fractions > 1)

  trajectory <- flip_edges(trajectory, milestone_network_toflip)

  # remove the root
  trajectory <- remove_root(trajectory)

  # tada!
  trajectory
}





#' Flip a set of edges of the milestone network
#'
#' @description
#' Note that this will remove associated roots, reroot the trajectory using [add_root()]
#'
#' @inheritParams common_param
#' @param milestone_network_toflip A dataframe with a from and to column, containing the subset of the milestone network #'
#' @keywords adapt_trajectory
#'
#' @export
flip_edges <- function(
  trajectory,
  milestone_network_toflip
) {
  assert_that(is(milestone_network_toflip, "data.frame"))
  assert_that(all(c("from", "to") %in% colnames(milestone_network_toflip)))
  assert_that(all(
    paste0(milestone_network_toflip$from, milestone_network_toflip$to) %in%
      paste0(trajectory$milestone_network$from, trajectory$milestone_network$to)
  ), msg = "All edges in the milestone_network_toflip should also be present in the trajectory milestone network")

  milestone_network_toflip <- milestone_network_toflip %>%
    select(from, to)

  # flip edge if from is later than to
  trajectory$milestone_network <- trajectory$milestone_network %>%
    left_join(milestone_network_toflip %>% mutate(flip = TRUE), c("from", "to")) %>%
    mutate(flip = ifelse(is.na(flip), FALSE, flip))

  # flip milestone network & progressions
  trajectory$progressions <- trajectory$progressions %>%
    left_join(trajectory$milestone_network %>% select(from, to, flip), c("from", "to")) %>%
    mutate(
      from2 = from,
      from = ifelse(flip, to, from),
      to = ifelse(flip, from2, to),
      percentage = ifelse(flip, 1-percentage, percentage)
    ) %>%
    select(-flip, -from2)

  trajectory$milestone_network <- trajectory$milestone_network %>%
    mutate(
      from2 = from,
      from = ifelse(flip, to, from),
      to = ifelse(flip, from2, to),
      directed = TRUE
    ) %>%
    select(-flip, -from2)

  trajectory
}
