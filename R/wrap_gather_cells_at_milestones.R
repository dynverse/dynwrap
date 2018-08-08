#' "Gather" cells to their closest milestones
#'
#' @param trajectory The trajectory
#' @export
gather_cells_at_milestones <- function(trajectory) {
  testthat::expect_true(is_wrapper_with_trajectory(trajectory))

  milestone_percentages <- trajectory$milestone_percentages %>%
    group_by(cell_id) %>%
    arrange(-percentage) %>%
    slice(1) %>%
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


