#' Group cells to their highest milestone
#' @param milestone_percentages milestone percentages dataframe
#' @export
get_cell_grouping <- function(milestone_percentages) {
  milestone_percentages %>% group_by(cell_id) %>%
    summarise(group_id=milestone_id[which.max(percentage)])
}

