#' Add information on overall differentially expressed features
#'
#' @param trajectory The trajectory
#' @param tde_overall A tibble containing a feature_id (character) and differentially_expressed (logical). Can also contain other columns, such as the p-value, q-value, log fold-change, etc
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
