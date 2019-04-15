#' Reorients the edges of the milestone network to the cell's RNA velocity vectors
#'
#' @inheritParams common_param
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
#' milestone_network <- tribble(
#'   ~from, ~to, ~length, ~directed,
#'   "B", "A", 1, TRUE,
#'   "B", "C", 1, TRUE
#' )
#' progressions <- tribble(
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
#' ) %>%
#'   add_trajectory(milestone_network = milestone_network, progressions = progressions)
#'
#' trajectory_oriented <- dynwrap::orient_topology_to_velocity(trajectory)
#'
#' # the edge is now correctly oriented
#' trajectory_oriented$milestone_network
orient_topology_to_velocity <- function(
  trajectory
) {
  # dummy proofing
  assert_that(is(trajectory, "dynwrap::with_trajectory"))
  assert_that(!is.null(trajectory$expression_projected))

  if (nrow(trajectory$divergence_regions)) {
    stop("Orienting topologies with divergence regions doesn't work yet")
  }

  # select waypoints along the trajectory topology
  waypoints <- select_waypoints(trajectory)

  # project waypoints into original expression space, using both the expression and the projected (RNA velocity) expression
  waypoint_expression <- project_waypoints(trajectory, trajectory$expression, waypoints)
  waypoint_expression_projected <- project_waypoints(trajectory, trajectory$expression_projected, waypoints)

  # calculate for each edge within the waypoint network the difference in expression
  waypoint_network_vectors <- pmap(waypoints$waypoint_network, function(from, to, ...) {
    waypoint_expression[to, ] - waypoint_expression[from, ]
  }) %>% do.call(rbind, .)

  # calculate the velocity vector for each waypoint
  assert_that(all(rownames(waypoint_expression_projected) == rownames(waypoint_expression)))
  assert_that(all(colnames(waypoint_expression_projected) == colnames(waypoint_expression)))
  waypoint_vectors <- waypoint_expression_projected - waypoint_expression

  # calculate correlation between:
  # * the difference in expression between each consecutive waypoint
  # * the RNA velocity vector at a particular waypoint
  waypoint_network_correlations <- pcor(
    t(as.matrix(waypoint_network_vectors)),
    t(as.matrix(waypoint_vectors[waypoints$waypoint_network$from, ]))
    , method = "spearman"
  )[1, ]

  # summarise the correlation for each edge in the milestone network
  milestone_network_correlations <- waypoints$waypoint_network %>%
    mutate(correlation = waypoint_network_correlations) %>%
    group_by(from_milestone_id, to_milestone_id) %>%
    summarise(
      correlation_sd = sd(correlation),
      correlation_mean = mean(correlation)
    ) %>%
    ungroup() %>%
    rename(from = from_milestone_id, to = to_milestone_id)

  # switch the directionality of edges based on the average correlation
  milestone_network_toflip <- milestone_network_correlations %>%
    filter(correlation_mean < 0) %>%
    select(from, to)

  trajectory <- flip_edges(trajectory, milestone_network_toflip)

  # remove the root
  trajectory <- remove_root(trajectory)

  # tada!
  trajectory
}





#' Flip a set of edges of the milestone network
#'
#' @description
#' Note that this will remove associated roots, reroot the trajectory using [root_trajectory()]
#'
#' @inheritParams common_param
#' @param milestone_network_toflip A dataframe with a from and to column, containing the subset of the milestone network #'
#' @keywords adapt_trajectory
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
