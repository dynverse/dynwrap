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
  # create milestone waypoints
  waypoint_milestone_percentages_milestones <- tibble(
    milestone_id = traj$milestone_ids,
    waypoint_id = paste0("W", milestone_id),
    percentage = 1
  )

  # create uniform progressions
  # waypoints which lie on a milestone will get a special name, so that they are the same between milestone network edges
  waypoint_progressions <- traj$milestone_network %>%
    mutate(percentage = map(length, ~c(seq(0, ., min(resolution, .))/., 1))) %>%
    select(-length, -directed) %>%
    unnest(percentage) %>%
    group_by(from, to, percentage) %>% # remove duplicate waypoints
    filter(row_number() == 1) %>%
    ungroup() %>%
    mutate(waypoint_id = case_when(
      percentage == 0 ~ paste0("W", from),
      percentage == 1 ~ paste0("W", to),
      TRUE ~ paste0("W", row_number())
    )
  )

  # create waypoint percentages from progressions
  waypoint_milestone_percentages <- waypoint_progressions %>%
    group_by(waypoint_id) %>%
    filter(row_number() == 1) %>%
    rename(cell_id = waypoint_id) %>%
    convert_progressions_to_milestone_percentages(
      "this argument is unnecessary, I can put everything I want in here!",
      traj$milestone_ids,
      traj$milestone_network,
      .
    ) %>%
    rename(waypoint_id = cell_id)

  # calculate distance
  waypoint_geodesic_distances <- compute_tented_geodesic_distances(traj, waypoint_milestone_percentages = waypoint_milestone_percentages)

  # also create network between waypoints
  waypoint_network <- waypoint_progressions %>%
    group_by(from, to) %>%
    mutate(from_waypoint = waypoint_id, to_waypoint = lead(waypoint_id, 1)) %>%
    drop_na() %>% ungroup() %>%
    select(from = from_waypoint, to = to_waypoint)

  # create waypoints and their properties
  waypoints <- waypoint_milestone_percentages %>%
    group_by(waypoint_id) %>%
    arrange(-percentage) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    mutate(milestone_id = ifelse(percentage == 1, milestone_id, NA)) %>%
    select(-percentage)

  lst(
    milestone_percentages = waypoint_milestone_percentages,
    progressions = waypoint_progressions,
    geodesic_distances = waypoint_geodesic_distances,
    waypoint_network,
    waypoints
  )
}
