#' Add a linear trajectory to a data wrapper
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
add_linear_trajectory <- function(
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
  milestone_ids <- c("milestone_start", "milestone_end")

  # construct milestone_network
  milestone_network <- data_frame(
    from = milestone_ids[[1]],
    to = milestone_ids[[2]],
    length = 1,
    directed = directed
  )

  # construct progressions
  progressions <- data_frame(
    cell_id = names(pseudotimes),
    from = milestone_ids[[1]],
    to = milestone_ids[[2]],
    percentage = pseudotimes
  )

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
