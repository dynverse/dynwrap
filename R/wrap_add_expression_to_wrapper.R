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
  testthat::expect_true(is_data_wrapper(data_wrapper))

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
  data_wrapper %>% extend_with(
    "dynwrap::with_expression",
    counts = counts,
    expression = expression,
    feature_info = feature_info,
    ...
  )
}

#' Test whether an object is a data_wrapper and has expression data
#'
#' @param object The object to be tested.
#'
#' @export
is_wrapper_with_expression <- function(object) {
  is_data_wrapper(object) && "dynwrap::with_expression" %in% class(object)
}



#' Get expression
#'
#' @param task The task
#' @param expression_source `expression`, `counts` or a matrix
#' @export
get_expression <- function(task, expression_source) {
  if (is.character(expression_source)) {
    if(!expression_source %in% names(task)) {stop("Expression source not in traj, did you run add_expression_to_wrapper?")}
    expression <- task[[expression_source]]
  } else if (is.matrix(expression_source)) {
    expression <- expression_source
  } else {
    stop("Invalid expression_source")
  }
  expression
}
