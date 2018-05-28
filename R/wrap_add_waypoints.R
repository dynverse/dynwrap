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
  # create uniform progressions
  generate_uniform_waypoint_progressions <- function(milestone_network) {
    milestone_network %>%
      mutate(percentage = map(length, ~c(seq(0, ., min(resolution, .))/., 1))) %>%
      select(-length, -directed) %>%
      unnest(percentage) %>%
      mutate(cell_id = paste0("W", row_number()))
  }
  waypoint_progressions <- generate_uniform_waypoint_progressions(traj$milestone_network)

  # create waypoint percentages from progressions
  waypoint_milestone_percentages <- waypoint_progressions %>%
    convert_progressions_to_milestone_percentages(
      "this argument is unnecessary, I can put everything I want in here!",
      traj$milestone_ids,
      traj$milestone_network,
      .
    ) %>% rename(waypoint_id = cell_id)

  # calculate distance
  waypoint_geodesic_distances <- compute_tented_geodesic_distances(traj, waypoint_milestone_percentages = waypoint_milestone_percentages)

  # also create network between waypoints

  # network between waypoints within an edge: just select successive waypoints
  waypoint_network_intra_edges <- waypoint_progressions %>%
    group_by(from, to) %>%
    mutate(from_waypoint = cell_id, to_waypoint = lead(cell_id, 1)) %>%
    drop_na() %>% ungroup() %>%
    select(from = from_waypoint, to = to_waypoint)

  # network between waypoints of different edges: select those waypoints with percentage == 1
  waypoint_network_inter_edges <- waypoint_milestone_percentages %>%
    filter(percentage == 1) %>%
    group_by(milestone_id) %>%
    filter(n() > 1) %>%
    summarise(waypoint_ids = list(waypoint_id)) %>%
    mutate(subnetwork = map(waypoint_ids, function(waypoint_ids) {
      links <- combn(waypoint_ids, 2, simplify=F)
      tibble(from = map_chr(links, ~.[[1]]), to = map_chr(links, ~.[[2]]))
      })
    ) %>% unnest(subnetwork) %>%
      select(from, to)

  waypoint_network <- bind_rows(waypoint_network_intra_edges, waypoint_network_inter_edges)

  # create waypoints and their properties
  waypoints <- waypoint_milestone_percentages %>%
    group_by(waypoint_id) %>%
    filter(waypoint_id > 0) %>%
    arrange(percentage) %>%
    filter(row_number() == 1) %>%
    mutate(milestone_id = ifelse(percentage == 1, NA, milestone_id)) %>%
    select(-percentage)

  lst(
    milestone_percentages = waypoint_milestone_percentages,
    progressions = waypoint_progressions,
    geodesic_distances = waypoint_geodesic_distances,
    waypoint_network,
    waypoints
  )
}
