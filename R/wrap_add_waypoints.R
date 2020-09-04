

#' @rdname add_waypoints
#'
#' @export
select_waypoints <- function(
  trajectory,
  n_waypoints = 200,
  trafo = sqrt,
  resolution = sum(trafo(trajectory$milestone_network$length))/n_waypoints,
  recompute = FALSE
) {
  assert_that(is_wrapper_with_trajectory(trajectory))

  if (!recompute && is(trajectory, "dynwrap::with_waypoints") && !is.null(trajectory$waypoints)) {
    return(trajectory$waypoints)
  }

  # create milestone waypoints
  waypoint_milestone_percentages_milestones <- tibble(
    milestone_id = trajectory$milestone_ids,
    waypoint_id = paste0("W", milestone_id),
    percentage = 1
  )

  # create uniform progressions
  # waypoints which lie on a milestone will get a special name, so that they are the same between milestone network edges
  waypoint_progressions <- trajectory$milestone_network %>%
    mutate(percentage = map(trafo(length), ~c(seq(0, ., min(resolution, .))/., 1))) %>%
    select(-length, -directed) %>%
    unnest(percentage) %>%
    group_by(from, to, percentage) %>% # remove duplicate waypoints
    filter(row_number() == 1) %>%
    ungroup() %>%
    mutate(
      waypoint_id = case_when(
        percentage == 0 ~ paste0("MILESTONE_BEGIN_W", from, "_", to),
        percentage == 1 ~ paste0("MILESTONE_END_W", from, "_", to),
        TRUE ~ paste0("W", row_number())
      )
    )

  # create waypoint percentages from progressions
  waypoint_milestone_percentages <- waypoint_progressions %>%
    group_by(waypoint_id) %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    rename(cell_id = waypoint_id) %>%
    convert_progressions_to_milestone_percentages(
      "this argument is unnecessary, I can put everything I want in here!",
      trajectory$milestone_ids,
      trajectory$milestone_network,
      .
    ) %>%
    rename(waypoint_id = cell_id)

  # calculate distance
  waypoint_geodesic_distances <- calculate_geodesic_distances(
    trajectory,
    waypoint_milestone_percentages = waypoint_milestone_percentages
  )[waypoint_progressions$waypoint_id, ]

  # also create network between waypoints
  waypoint_network <- waypoint_progressions %>%
    group_by(from, to) %>%
    mutate(from_waypoint = waypoint_id, to_waypoint = lead(waypoint_id, 1)) %>%
    drop_na() %>%
    ungroup() %>%
    select(from = from_waypoint, to = to_waypoint, from_milestone_id = from, to_milestone_id = to)

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

#' Add or create waypoints to a trajectory
#'
#' Waypoints are points along the trajectory, which do not necessarily correspond to cells. They are selected in such a way that all parts of the trajectory are covered
#'
#' @inheritParams common_param
#' @param n_waypoints The number of waypoints
#' @param trafo Transformation function of the edge lengths
#' @param resolution The resolution of the waypoints, measured in the same units as the lengths of the milestone network edges, will be automatically computed using n_waypoints
#' @param recompute Force recompute
#'
#' @return
#' **`add_waypoints`** returns the trajectory with *waypoints* added, which is a list containing:
#' - *milestone_percentages* and *progressions*: The milestone percentages and progressions of each waypoint, in the same format as the cell equivalents (see [add_trajectory()]) but with a *waypoint_id* column instead of a *cell_id* column
#' - *geodesic_distances*: a matrix with the geodesic distance of each waypoint (rows) to every cell (columns)
#' - *waypoint_network*: a dataframe containing the network between consecutive waypoints, it contains information on the connected waypoints (*from* and *to*) and the edge on which they reside (*from_milestone_id* and *to_milestone_id*)
#' - *waypoints*: the waypoint identifiers
#'
#' **`select_waypoints` returns the list as mentioned in `add_waypoints`
#'
#' @keywords adapt_trajectory
#'
#' @export
add_waypoints <- inherit_default_params(select_waypoints, function(
  trajectory,
  n_waypoints = 200,
  trafo = sqrt,
  resolution = sum(trafo(trajectory$milestone_network$length))/n_waypoints,
  recompute = FALSE
) {
  assert_that(is_wrapper_with_trajectory(trajectory))

  waypoints <- with(trajectory, select_waypoints(
    trajectory,
    n_waypoints,
    trafo,
    resolution,
    recompute
  ))

  # create output structure
  trajectory %>% extend_with(
    "dynwrap::with_waypoints",
    waypoints = waypoints
  )
})

#' @rdname add_waypoints
#'
#' @export
is_wrapper_with_waypoints <- function(trajectory) {
  is_wrapper_with_trajectory(trajectory) && "dynwrap::with_waypoints" %in% class(trajectory)
}
