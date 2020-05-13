#' Gather cells to their closest milestones
#'
#' Cells will be moved to their closest milestones.
#'
#' @inheritParams common_param
#'
#' @return A trajectory where cells where moved to the closest milestone, the milestone_percentages and progressions will be adapated.
#'
#' @keywords adapt_trajectory
#'
#' @examples
#' trajectory <- example_trajectory
#' trajectory <- gather_cells_at_milestones(trajectory)
#' head(trajectory$milestone_percentages)
#'
#' @export
gather_cells_at_milestones <- function(trajectory) {
  assert_that(is_wrapper_with_trajectory(trajectory))

  milestone_percentages <-
    trajectory$milestone_percentages %>%
    group_by(cell_id) %>%
    slice(which.max(percentage)) %>%
    mutate(percentage = 1) %>%
    ungroup()

  trajectory %>%
    add_trajectory(
      milestone_network = trajectory$milestone_network,
      divergence_regions = trajectory$divergence_regions,
      milestone_percentages = milestone_percentages,
      trajectory_cell_positioning = "milestones"
    )
}


