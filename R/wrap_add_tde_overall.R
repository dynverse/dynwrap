#' Add information on overall differentially expressed features
#'
#' To calculate differential expression within trajectories, check out the [dynfeature](https://github.com/dynverse/dynfeature) package.
#'
#' @inheritParams common_param
#' @param tde_overall A dataframe containing the *feature_id*, and some other columns including whether it is differentially expressed (*differentially_expressed*), the rank of differential expression among all other features (*rank*), the p-value (*pval*) or corrected value (*qval*), and the log-fold change (*lfc*).
#'
#' @return A trajectory containing *tde_overall*, a dataframe containing the *feature_id*, and some other columns including whether it is differentially expressed (*differentially_expressed*), the rank of differential expression among all other features (*rank*), the p-value (*pval*) or corrected value (*qval*), and the log-fold change (*lfc*).
#'
#' @examples
#' trajectory <- example_trajectory
#' tde_overall <- tibble::tibble(
#'   feature_id = trajectory$feature_info$feature_id,
#'   differentially_expressed = sample(c(TRUE, FALSE), length(feature_id), replace = TRUE)
#' )
#' trajectory <- add_tde_overall(trajectory, tde_overall)
#' trajectory$tde_overall
#'
#' @keywords adapt_trajectory
#'
#' @export
add_tde_overall <- function(trajectory, tde_overall) {
  feature_ids <- colnames(get_expression(trajectory))

  # check format of diffexp_overall
  assert_that(!is.null(names(tde_overall)))
  assert_that(is_tibble(tde_overall))
  assert_that(is.logical(tde_overall$differentially_expressed))
  assert_that(is.character(tde_overall$feature_id))
  assert_that(setequal(tde_overall$feature_id, feature_ids))

  trajectory$tde_overall <- tde_overall
  trajectory
}
