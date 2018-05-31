#' Add a cyclic trajectory to a data wrapper
#'
#' This function will generate the milestone_network and progressions.
#'
#' @param data_wrapper A data wrapper to extend upon.
#' @param pseudotimes A named vector of pseudo times.
#' @param directed Whether or not the directionality of the pseudotime is predicted.
#' @param do_scale_minmax Whether or not to scale the pseudotimes between 0 and 1.
#'   Otherwise, will assume the values are already within that range.
#' @param ... extra information to be stored in the wrapper.
#'
#' @export
#'
#' @importFrom testthat expect_is expect_true expect_named
add_cyclic_trajectory <- function(
  data_wrapper,
  pseudotimes,
  directed = FALSE,
  do_scale_minmax = TRUE,
  ...
) {
  # check data wrapper
  testthat::expect_true(is_data_wrapper(data_wrapper))

  # check names of pseudotimes
  cell_ids <- data_wrapper$cell_ids
  testthat::expect_is(pseudotimes, "numeric")
  testthat::expect_named(pseudotimes)
  testthat::expect_true(all(names(pseudotimes) %in% cell_ids))

  # scale pseudotimes
  if (do_scale_minmax) {
    pseudotimes <- scale_minmax(pseudotimes)
  } else {
    testthat::expect_true(all(0 <= pseudotimes & pseudotimes <= 1))
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
    time = 3 * pseudotimes,
    cell_id = names(pseudotimes)
  ) %>%
    mutate(edge_id = ifelse(time <= 1, 1L, ifelse(time <= 2, 2L, 3L))) %>%
    left_join(milestone_network, by = "edge_id") %>%
    mutate(percentage = time - (edge_id - 1)) %>%
    select(cell_id, from, to, percentage)

  milestone_network <- milestone_network %>%
    select(from, to, length, directed)

  # return output
  add_trajectory(
    data_wrapper = data_wrapper,
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    divergence_regions = NULL,
    progressions = progressions,
    pseudotimes = pseudotimes,
    ...
  )
}
