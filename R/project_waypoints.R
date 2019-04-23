#' Project waypoints within a trajectory (e.g. milestones) into a space defined by cells (e.g. expression)
#'
#' @inheritParams common_param
#' @param space A matrix with cells in rows and different dimensions in the columns. This is typically an expression matrix or a dimensionality reduction
#' @param waypoints A list containing:
#'   * `waypoints`: a dataframe containing in the very least the waypoint_id
#'   * `milestone_percentages`: the positions of waypoints withing the trajectory
#'   * `geodesic_distances`: matrix with precalculated geodesic distances between waypoints (rows) and cells (columns), optional
#'   A set of suitable waypoints can be created [dynwrap::select_waypoints()]
#' @param trajectory_projection_sd The standard deviation of the gaussian kernel
#'
#' @return A matrix with waypoints in rows and dimensions in the columns.
#'
#' @importFrom stats dnorm
#' @export
project_waypoints <- function(
  trajectory,
  space,
  waypoints = dynwrap::select_waypoints(trajectory),
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
    )[unique(waypoints$milestone_percentages$waypoint_id), , drop = F]
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



#' Project a trajectory onto a dimensionality reduction using waypoints
#'
#' @inheritParams common_param
#' @inheritParams add_dimred
#'
#' @return A list containing dimred_segment_points and dimred_segment_progressions, which can be given to [add_dimred()]
#'
#' @export
project_trajectory <- function(trajectory, dimred, waypoints = select_waypoint(trajectory)) {
  waypoint_points <- project_waypoints(trajectory, dimred, waypoints = waypoints)

  lst(
    dimred_segment_points = waypoint_points,
    dimred_segment_progressions = waypoints$progressions %>% select(from, to, percentage)
  )
}



#' Project milestones onto a dimensionality reduction
#'
#' @inheritParams common_param
#' @inheritParams add_dimred
#'
#' @return A list containing dimred_segment_points and dimred_segment_progressions, which can be given to [add_dimred()]
#'
#' @export
project_milestones <- function(trajectory, dimred) {
  waypoints <- lst(
    milestone_percentages = tibble(
      waypoint_id = trajectory$milestone_ids,
      milestone_id = waypoint_id,
      percentage = 1
    )
  )
  waypoint_points <- project_waypoints(trajectory, dimred, waypoints = waypoints)

  waypoint_points
}
