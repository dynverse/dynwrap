#' Add waypoints to a wrapped object with trajectory
#'
#' @param object Wrapper with trajectory
#' @inheritParams select_waypoints
#'
#' @importFrom testthat expect_true
#'
#' @export
add_waypoints_to_wrapper <- function(object, resolution = 0.1) {
  testthat::expect_true(is_wrapper_with_trajectory(object))

  waypoints <- with(object, select_waypoints(
    object,
    resolution
  ))

  # create output structure
  object %>% extend_with(
    "dynwrap::with_waypoints",
    waypoints = waypoints
  )
}

#' Test whether an object is a data_wrapper and waypoints
#'
#' @param object The object to be tested.
#'
#' @export
is_wrapper_with_waypoints <- function(object) {
  is_wrapper_with_trajectory(object) && "dynwrap::with_waypoints" %in% class(object)
}

#' Select the waypoints
#'
#' Waypoints are spread equally over the whole trajectory
#'
#' @inheritParams add_trajectory_to_wrapper
#' @param resoltion The resolution of the waypoints, measured in the same units as the lengths of the milestone network edges
#'
#' @export
select_waypoints <- function(
  traj,
  num_cells_selected = 100
) {
  generate_uniform_waypoint_progressions <- function(milestone_network) {
    resolution <- 0.1
    milestone_network %>%
      mutate(percentage = map(length, ~seq(0, ., min(resolution, .))/.)) %>%
      select(-length, -directed) %>%
      unnest(percentage) %>%
      mutate(cell_id = paste0("W", row_number()))
  }
  waypoint_milestone_progressions <- generate_uniform_waypoint_progressions(traj$milestone_network)
  waypoint_milestone_percentages <- waypoint_milestone_progressions %>%
    convert_progressions_to_milestone_percentages(
      "this parameter is unnecessary why is this even here",
      traj$milestone_ids,
      traj$milestone_network,
      .
    ) %>% rename(waypoint_id = cell_id)

  waypoint_geodesic_distances <- compute_tented_geodesic_distances(traj, waypoint_milestone_percentages = waypoint_milestone_percentages)

  lst(
    milestone_percentages = waypoint_milestone_percentages,
    milestone_progressions = waypoint_milestone_progressions,
    geodesic_distances = waypoint_geodesic_distances
  )
}
