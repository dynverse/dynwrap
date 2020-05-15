#' Project waypoints of a trajectory (e.g. milestones) into a space defined by cells (e.g. expression or a dimensionality reduction)
#'
#' This will first calculate the geodesic distance of each cell to the waypoint. This distance is used as a weight
#'
#' @inheritParams common_param
#' @param space A matrix with cells in rows and different dimensions in the columns. This is typically an expression matrix or a dimensionality reduction
#' @param waypoints A set of waypoints, which can be created by [`select_waypoints()`][add_waypoints()]. It is a list containing:
#'   * `waypoints`: a dataframe containing in the very least the waypoint_id
#'   * `milestone_percentages`: the positions of waypoints withing the trajectory
#'   * `geodesic_distances`: matrix with precalculated geodesic distances between waypoints (rows) and cells (columns), optional
#' @param trajectory_projection_sd The standard deviation of the gaussian kernel
#'
#' @return A matrix in which the waypoints (rows) were projected into a new space defined by the same number of dimensions (columns) as in the `space` argument
#'
#' @importFrom stats dnorm
#' @export
project_waypoints <- function(
  trajectory,
  space,
  waypoints = select_waypoints(trajectory),
  trajectory_projection_sd = sum(trajectory$milestone_network$length) * 0.05
) {
  assert_that(!is.null(space))

  # calculate or check geodesic distances between waypoints and cells
  if ("geodesic_distances" %in% names(waypoints)) {
    assert_that(all(rownames(space) %in% colnames(waypoints$geodesic_distances)))
  } else {
    waypoints$geodesic_distances <- calculate_geodesic_distances(
      trajectory,
      waypoint_milestone_percentages = waypoints$milestone_percentages
    )[unique(waypoints$milestone_percentages$waypoint_id), , drop = FALSE]
  }

  # apply kernel on geodesic distances
  # in theory, many kernels are possible here, but for now this is fixed to a normal kernel
  weights <- waypoints$geodesic_distances %>%
    stats::dnorm(sd = trajectory_projection_sd)
  assert_that(all(!is.na(weights)))

  weights <- weights / rowSums(weights)

  # prepare space
  weights <- weights[, rownames(space)]
  assert_that(all(colnames(weights) == rownames(space)))

  # calculate positions
  projected_space <- weights %*% space

  projected_space
}



#' Project a trajectory onto a dimensionality reduction
#'
#' @inheritParams common_param
#' @inheritParams add_dimred
#' @inheritParams project_waypoints
#' @param dimred The dimensionality reduction of the cells. A matrix with the positions of cells (rows) in the dimensions (columns)
#'
#' @return A list containing
#' - *dimred_segment_points*: The dimensionality reduction of a set of points along the trajectory. A matrix with the position of points (rows) in the dimensions (columns)
#' - *dimred_segment_progressions* The progressions of the points. A dataframe containing the *from* and *to* milestones, and their *progression*. Has the same number of rows as *dimred_segment_points*
#' - *dimred_milestones*: The dimensionality reduction of the milestones. A matrix with the position of milestones (rows) in the dimensions (columns)
#'
#' These objects can be given to [add_dimred()]
#'
#' @seealso [add_dimred()]
#'
#' @export
project_trajectory <- function(
  trajectory,
  dimred,
  waypoints = select_waypoints(trajectory),
  trajectory_projection_sd = sum(trajectory$milestone_network$length) * 0.05
) {
  dimred_segment_points <- project_waypoints(
    trajectory,
    dimred,
    waypoints = waypoints,
    trajectory_projection_sd = trajectory_projection_sd
  )
  dimred_milestones <- project_milestones(
    trajectory,
    dimred,
    trajectory_projection_sd = trajectory_projection_sd
  )

  lst(
    dimred_segment_points = dimred_segment_points,
    dimred_segment_progressions = waypoints$progressions %>% select(from, to, percentage),
    dimred_milestones = dimred_milestones
  )
}



#' @rdname project_trajectory
#'
#' @export
project_milestones <- function(trajectory, dimred, trajectory_projection_sd = sum(trajectory$milestone_network$length) * 0.05) {
  waypoints <- lst(
    milestone_percentages = tibble(
      waypoint_id = trajectory$milestone_ids,
      milestone_id = waypoint_id,
      percentage = 1
    )
  )
  project_waypoints(
    trajectory,
    dimred,
    waypoints = waypoints,
    trajectory_projection_sd = trajectory_projection_sd
  )
}
