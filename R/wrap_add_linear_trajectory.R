#' Constructs a linear trajectory using the pseudotime values of each cell.
#'
#' This function will generate the milestone_network and progressions.
#'
#' @param model The model to which a linear trajectory will be added.
#' @param pseudotime A named vector of pseudo times.
#' @param directed Whether or not the directionality of the pseudotime is predicted.
#' @param do_scale_minmax Whether or not to scale the pseudotime between 0 and 1.
#'   Otherwise, will assume the values are already within that range.
#' @param ... extra information to be stored in the wrapper.
#'
#' @return The trajectory model
#'
#' @export
#'
#' @importFrom testthat expect_is expect_true expect_named
add_linear_trajectory <- function(
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
  milestone_ids <- c("milestone_begin", "milestone_end")

  # construct milestone_network
  milestone_network <- data_frame(
    from = milestone_ids[[1]],
    to = milestone_ids[[2]],
    length = 1,
    directed = directed
  )

  # construct progressions
  progressions <- data_frame(
    cell_id = names(pseudotime),
    from = milestone_ids[[1]],
    to = milestone_ids[[2]],
    percentage = pseudotime
  )

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
