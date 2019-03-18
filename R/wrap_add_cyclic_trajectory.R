#' Constructs a circular trajectory using the pseudotime values of each cell.
#'
#' This function will generate the milestone_network and progressions.
#'
#' @param model The model to which a cyclic trajectory will be added.
#' @param pseudotime A named vector of pseudo times.
#' @param directed Whether or not the directionality of the pseudotime is predicted.
#' @param do_scale_minmax Whether or not to scale the pseudotime between 0 and 1.
#'   Otherwise, will assume the values are already within that range.
#' @param ... extra information to be stored in the wrapper.
#'
#' @keywords create_trajectory
#'
#' @export
#'
#' @return The trajectory model
#'
#' @importFrom testthat expect_is expect_true expect_named
add_cyclic_trajectory <- function(
  model,
  pseudotime,
  directed = FALSE,
  do_scale_minmax = TRUE,
  ...
) {
  # check data wrapper
  testthat::expect_true(is_data_wrapper(model))

  pseudotime <- process_pseudotime(model, pseudotime)

  # scale pseudotime
  if (do_scale_minmax) {
    pseudotime <- scale_minmax(pseudotime)
  } else {
    testthat::expect_true(all(0 <= pseudotime & pseudotime <= 1))
  }

  # construct milestones
  milestone_ids <- c("A", "B", "C")

  # construct milestone_network
  milestone_network <- tibble(
    from = milestone_ids,
    to = milestone_ids[c(2,3,1)],
    directed = directed,
    length = 1,
    edge_id = seq_along(milestone_ids)
  )

  # construct progressions
  progressions <- tibble(
    time = 3 * pseudotime,
    cell_id = names(pseudotime)
  ) %>%
    mutate(edge_id = ifelse(time <= 1, 1L, ifelse(time <= 2, 2L, 3L))) %>%
    left_join(milestone_network, by = "edge_id") %>%
    mutate(percentage = time - (edge_id - 1)) %>%
    select(cell_id, from, to, percentage)

  milestone_network <- milestone_network %>%
    select(from, to, length, directed)

  # return output
  add_trajectory(
    model = model,
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    divergence_regions = NULL,
    progressions = progressions,
    pseudotime = pseudotime,
    ...
  )
}
