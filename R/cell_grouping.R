#' Group cells to their highest milestone
#' @param milestone_percentages milestone percentages dataframe
#' @export
get_cell_grouping <- function(milestone_percentages) {
  milestone_percentages %>% group_by(cell_id) %>%
    summarise(group_id=milestone_id[which.max(percentage)])
}


#' Calculate mean expression levels for every cell group
#' @param counts matrix
#' @param cell_grouping grouping of cells dataframe
#' @export
group_counts <- function(counts, cell_grouping) {
  cell_grouping <- cell_grouping %>% slice(match(rownames(counts), as.character(cell_id)))
  counts <- counts[as.character(cell_grouping$cell_id), ]

  counts %>% as.data.frame() %>% split(cell_grouping$group_id) %>% map(~apply(., 2, mean)) %>% do.call(rbind, .)
}
