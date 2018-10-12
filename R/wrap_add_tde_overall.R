#' Add information on overall differentially expressed features
#'
#' @param trajectory The trajectory
#' @param tde_overall A tibble containing a feature_id (character) and differentially_expressed (logical). Can also contain other columns, such as the p-value, q-value, log fold-change, etc
#' @export
add_tde_overall <- function(trajectory, tde_overall) {
  feature_ids <- colnames(get_expression(trajectory))

  # check format of diffexp_overall
  testthat::expect_named(tde_overall)
  testthat::expect_true(is_tibble(tde_overall))
  testthat::expect_is(tde_overall$differentially_expressed, "logical")
  testthat::expect_is(tde_overall$feature_id, "character")
  testthat::expect_setequal(tde_overall$feature_id, feature_ids)

  trajectory$tde_overall <- tde_overall
  trajectory
}
