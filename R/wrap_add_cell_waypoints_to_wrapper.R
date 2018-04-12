#' Add cell waypoints to a wrapped object with trajectory
#'
#' @param object Wrapper with trajectory
#' @param num_cells_selected About the number of cells selected as waypoints
#'
#' @importFrom testthat expect_true
#'
#' @export
add_cell_waypoints_to_wrapper <- function(object, num_cells_selected = 100) {
  testthat::expect_true(is_wrapper_with_trajectory(object))

  object$waypoint_cells <- select_waypoint_cells(
    milestone_ids = object$milestone_ids,
    milestone_network = object$milestone_network,
    milestone_percentages = object$milestone_percentages,
    progressions = object$progressions,
    divergence_regions = object$divergence_regions,
    num_cells_selected = num_cells_selected
  )

  class(object) <- c("dynwrap::with_cell_waypoints", class(object))

  object
}

#' Test whether an object is a data_wrapper and cell waypoints
#'
#' @param object The object to be tested.
#'
#' @export
is_wrapper_with_waypoint_cells <- function(object) {
  is_wrapper_with_trajectory(object) && "dynwrap::with_cell_waypoints" %in% class(object)
}
