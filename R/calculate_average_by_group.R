#' Calculate mean values per cell group
#'
#' @param x A matrix. One row for every cell; one column for every feature. The rows must be named.
#' @param cell_grouping A data frame denoting the grouping of the cells.
#'  Format: `data_frame(cell_id = character(), group_id = character())`.
#'
#' @export
#'
#' @examples
#' data(example_dataset)
#' calculate_average_by_group(
#'   x = example_dataset$expression,
#'   cell_grouping = example_dataset$prior_information$groups_id
#' )
calculate_average_by_group <- function(x, cell_grouping) {
  milestone_percentages <-
    cell_grouping %>%
    mutate(percentage = 1) %>%
    rename(milestone_id = group_id)

  calculate_average_by_milestone_percentages(x, milestone_percentages)
}
