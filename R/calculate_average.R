#' Calculate average values of a matrix
#'
#' `calculate_average_by_group` will calculate an average value per group, given a matrix with cells in the rows and some features in the columns (e.g. expression matrix)
#'
#' @param x A matrix. One row for every cell; one column for every feature. The rows must be named.
#' @param cell_grouping A data frame denoting the grouping of the cells.
#'  Format: `tibble(cell_id = character(), group_id = character())`.
#'
#' @return A matrix containing for each feature (column) the average
#'
#' @keywords derive_trajectory
#'
#' @export
#'
#' @examples
#' calculate_average_by_group(
#'   x = example_trajectory$expression,
#'   cell_grouping = example_trajectory$prior_information$groups_id
#' )
#'
#' @rdname calculate_average
calculate_average_by_group <- function(x, cell_grouping) {
  milestone_percentages <-
    cell_grouping %>%
    mutate(percentage = 1) %>%
    rename(milestone_id = group_id)

  calculate_average_by_milestone_percentages(x, milestone_percentages)
}

calculate_average_by_milestone_percentages <- function(x, milestone_percentages) {
  # cast milestone percentages to matrix
  milpct_m <- milestone_percentages %>%
    reshape2::acast(cell_id ~ milestone_id, value.var = "percentage", fill = 0, fun.aggregate = sum)

  stat <- colSums(milpct_m)
  stat[stat == 0] <- 1
  milpct_m <- sweep(milpct_m, 2, stat, "/")

  # check data objects
  assert_that(all.equal(sort(rownames(x)), sort(rownames(milpct_m))))

  # get same order
  milpct_m <- milpct_m[rownames(x), , drop = F]

  t(milpct_m) %*% x
}
