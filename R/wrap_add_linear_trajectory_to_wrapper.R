#' Add a linear trajectory to a data wrapper
#'
#' This function will generate the milestone_network and progressions.
#'
#' @param data_wrapper A data wrapper to extend upon.
#' @param pseudotimes A named vector of pseudo times.
#' @param ... extra information to be stored in the wrapper.
#'
#' @export
#'
#' @importFrom testthat expect_is expect_true
add_linear_trajectory_to_wrapper <- function(
  data_wrapper,
  pseudotimes,
  ...
) {
  # check data wrapper
  testthat::expect_is(data_wrapper, "dynutils::data_wrapper")

  # check names of pseudotimes
  cell_ids <- data_wrapper$cell_ids
  testthat::expect_is(pseudotimes, "numeric")
  testthat::expect_true(all(names(pseudotimes) %in% cell_ids))

  # scale pseudotimes
  pseudotimes <- scale_minmax(pseudotimes)

  # construct milestones
  milestone_ids <- c("milestone_start", "milestone_end")

  # construct milestone_network
  milestone_network <- data_frame(
    from = milestone_ids[[1]],
    to = milestone_ids[[2]],
    length = 1,
    directed = FALSE
  )

  # construct progressions
  progressions <- data_frame(
    cell_id = names(pseudotimes),
    from = milestone_ids[[1]],
    to = milestone_ids[[2]],
    percentage = pseudotimes
  )

  # return output
  add_trajectory_to_wrapper(
    data_wrapper = data_wrapper,
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    divergence_regions = NULL,
    progressions = progressions,
    pseudotimes = pseudotimes,
    ...
  )
}
