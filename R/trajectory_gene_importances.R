
#' Attempt at calculating gene importances per milestone
#'
#' Uses the feature importance measures of \code{\link[ranger]{ranger}}.
#'
#' @param traj A trajectory object containing expression values and a trajectory.
#' @param mtry A function for determining the mtry parameter.
#' @param num_trees The number of trees to use.
#' @param num_cores The number of cores to use.
#'
#' @importFrom reshape2 acast
#' @importFrom ranger ranger
#'
#' @export
trajectory_gene_importances <- function(
  traj,
  mtry = function(x) ncol(x) * .01,
  num_trees = 10000,
  num_cores = 1
) {
  testthat::expect_true(is_wrapper_with_expression(traj))
  testthat::expect_true(is_wrapper_with_trajectory(traj))

  cell_ids <- traj$cell_ids
  expression <- traj$expression

  if (is.function(expression)) {
    expression <- expression()
  }

  milenet_m <- traj$milestone_percentages %>%
    reshape2::acast(cell_id ~ milestone_id, value.var = "percentage", fill = 0) %>%
    expand_matrix(rownames = cell_ids)

  milenet_m <- milenet_m[,apply(milenet_m, 2, function(x) length(unique(x)) > 1)]

  importances <- map_df(seq_len(ncol(milenet_m)), function(i) {
    data <-
      milenet_m[,i, drop=F] %>%
      magrittr::set_colnames("PREDICT") %>%
      cbind(expression) %>%
      as.data.frame()
    importance <- ranger::ranger(
      dependent.variable.name = "PREDICT",
      data = data,
      mtry = mtry(expression),
      num.trees = num_trees,
      num.threads = num_cores,
      importance = "impurity"
    )$variable.importance

    data_frame(milestone_id = colnames(milenet_m)[[i]], feature_id = names(importance), importance)
  })

  importances %>% arrange(desc(importance))
}
