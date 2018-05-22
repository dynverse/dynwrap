#' Add waypoints to a wrapped traj with trajectory
#'
#' @inheritParams select_waypoints
#' @importFrom testthat expect_true
#'
#' @export
add_waypoints <- function(
  traj,
  n_waypoints = 100,
  resolution = sum(traj$milestone_network$length)/n_waypoints
) {
  testthat::expect_true(is_wrapper_with_trajectory(traj))

  waypoints <- with(traj, select_waypoints(
    traj,
    n_waypoints,
    resolution
  ))

  # create output structure
  traj %>% extend_with(
    "dynwrap::with_waypoints",
    waypoints = waypoints
  )
}

#' Test whether an traj is a data_wrapper and waypoints
#'
#' @param traj The traj to be tested.
#'
#' @export
is_wrapper_with_waypoints <- function(traj) {
  is_wrapper_with_trajectory(traj) && "dynwrap::with_waypoints" %in% class(traj)
}

#' Select the waypoints
#'
#' Waypoints are spread equally over the whole trajectory
#'
#' @param traj Wrapper with trajectory
#' @param n_waypoints The number of waypoints
#' @param resolution The resolution of the waypoints, measured in the same units as the lengths of the milestone network edges, will be automatically computed using n_waypoints
#'
#' @export
select_waypoints <- function(
  traj,
  n_waypoints = 100,
  resolution = sum(traj$milestone_network$length)/n_waypoints
) {
  generate_uniform_waypoint_progressions <- function(milestone_network) {
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
