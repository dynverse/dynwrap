#' Add count and normalised expression values to a data wrapper
#'
#' @param data_wrapper A data wrapper to extend upon.
#' @param counts The counts.
#' @param expression the normalised expression values.
#' @param feature_info Optional meta-information pertaining the features.
#' @param ... extra information to be stored in the wrapper
#'
#' @export
#'
#' @importFrom testthat expect_equal expect_is
add_expression_to_wrapper <- function(
  data_wrapper,
  counts,
  expression,
  feature_info = NULL,
  ...
) {
  testthat::expect_is(data_wrapper, "dynutils::data_wrapper")

  cell_ids <- data_wrapper$cell_ids

  testthat::expect_is(counts, "matrix")
  testthat::expect_is(expression, "matrix")
  testthat::expect_equal(rownames(counts), cell_ids)
  testthat::expect_equal(rownames(expression), cell_ids)
  testthat::expect_equal(colnames(expression), colnames(counts))

  if (!is.null(feature_info)) {
    testthat::expect_is(feature_info, "data.frame")
    testthat::expect_equal(colnames(counts), feature_info$feature_id)
    testthat::expect_equal(colnames(expression), feature_info$feature_id)
  }

  # create output structure
  out <- c(
    data_wrapper,
    list(
      counts = counts,
      expression = expression,
      feature_info = feature_info,
      ...
    ))
  class(out) <- c("dynutils::with_expression", class(data_wrapper))
  out
}

#' Test whether an object is a data_wrapper and has expression data
#'
#' @param object The object to be tested.
#'
#' @export
is_wrapper_with_expression <- function(object) {
  is_data_wrapper(object) && "dynutils::with_expression" %in% class(object)
}
