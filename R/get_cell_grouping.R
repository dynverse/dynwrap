#' Group cells to their highest milestone
#'
#' @param milestone_percentages A data frame of milestone percentages.
#'  Format: `data_frame(cell_id = character(), milestone_id = character(), percentage = numeric())`.
#'
#' @export
get_cell_grouping <- function(milestone_percentages) {
  milestone_percentages %>%
    group_by(cell_id) %>%
    summarise(group_id = milestone_id[which.max(percentage)])
}

