#' Calculate mean values per cell group
#'
#' @param x A matrix. One row for every cell; one column for every feature. The rows must be named.
#' @param cell_grouping A data frame denoting the grouping of the cells.
#'  Format: `data_frame(cell_id = character(), group_id = character())`.
#'
#' @export
calculate_average_by_group <- function(x, cell_grouping) {
  milestone_percentages <- cell_grouping %>% mutate(percentage = 1) %>% rename(milestone_id = group_id)
  calculate_average_by_milestone_percentages(x, milestone_percentages)
}

#' Calculate mean values by milestone percentages
#'
#' @param x A matrix. One row for every cell; one column for every feature. The rows must be named.
#' @param milestone_percentages A data frame of milestone percentages.
#'  Format: `data_frame(cell_id = character(), milestone_id = character(), percentage = numeric())`.
#'
#' @importFrom reshape2 acast
#' @importFrom testthat expect_equal expect_true
#'
#' @export
calculate_average_by_milestone_percentages <- function(x, milestone_percentages) {
  # cast milestone percentages to matrix
  milpct_m <- milestone_percentages %>%
    reshape2::acast(cell_id ~ milestone_id, value.var = "percentage", fill = 0, fun.aggregate = sum)
  stat <- colSums(milpct_m)
  stat[stat == 0] <- 1
  milpct_m <- sweep(milpct_m, 2, stat, "/")

  # check data objects
  testthat::expect_equal(sort(rownames(x)), sort(rownames(milpct_m)))

  # get same order
  milpct_m <- milpct_m[rownames(x), , drop = F]

  t(milpct_m) %*% x
}

