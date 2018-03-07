#' Add a dimensionality reductio n to a data wrapper
#'
#' @param data_wrapper A data wrapper to extend upon.
#' @param dimred The dimensionality reduction matrix.
#' @param dimred_milestones An optional dimensionality reduction of the milestones.
#' @param dimred_trajectory_segments An optional dimensionality reduction of the trajectory segments.
#' @param ... extra information to be stored in the wrapper
#'
#' @export
#'
#' @importFrom testthat expect_equal expect_is expect_true
add_dimred_to_wrapper <- function(
  data_wrapper,
  dimred,
  dimred_milestones = NULL,
  dimred_trajectory_segments = NULL,
  ...
) {
  testthat::expect_is(data_wrapper, "dynutils::data_wrapper")

  cell_ids <- data_wrapper$cell_ids

  testthat::expect_is(dimred, "matrix")
  testthat::expect_equal(rownames(dimred), cell_ids)

  if (!is.null(dimred_milestones)) {
    testthat::expect_true(is_wrapper_with_trajectory(data_wrapper))

    milestone_ids <- data_wrapper$milestone_ids
    testthat::expect_is(dimred_milestones, "matrix")
    testthat::expect_equal(rownames(dimred_milestones), milestone_ids)
    testthat::expect_equal(colnames(dimred_milestones), colnames(dimred))
  }

  if (!is.null(dimred_trajectory_segments)) {
    testthat::expect_is(dimred_trajectory_segments, "matrix")
    expected_colnames <- c(
      paste0("from_", colnames(dimred)),
      paste0("to_", colnames(dimred))
    )
    testthat::expect_equal(colnames(dimred_trajectory_segments), expected_colnames)
  }

  # create output structure
  out <- c(
    data_wrapper,
    list(
      dimred = dimred,
      dimred_milestones = dimred_milestones,
      dimred_trajectory_segments = dimred_trajectory_segments,
      ...
    ))
  class(out) <- c("dynutils::with_dimred", class(data_wrapper))
  out
}

#' Test whether an object is a data_wrapper and has dimred data
#'
#' @param object The object to be tested.
#'
#' @export
is_wrapper_with_dimred <- function(object) {
  is_data_wrapper(object) && "dynutils::with_dimred" %in% class(object)
}
