#' Add count and normalised expression values to a model
#'
#' @param model The model to which expression will be added
#' @param counts The counts with genes in columns and cells in rows
#' @param expression The normalised expression values with genes in columns and cells in rows
#' @param feature_info Optional meta-information of the features, a data.frame with at least feature_id as column
#' @param ... extra information to be stored in the model
#' @param expression_source The source of expression, can be "counts", "expression", an expression matrix, or another model which contains expression
#'
#' @export
#'
#' @importFrom testthat expect_equal expect_is
add_expression <- function(
  model,
  counts,
  expression,
  feature_info = NULL,
  ...
) {
  testthat::expect_true(is_data_wrapper(model))

  cell_ids <- model$cell_ids

  testthat::expect_is(counts, "matrix")
  testthat::expect_is(expression, "matrix")
  testthat::expect_equal(rownames(counts), cell_ids)
  testthat::expect_equal(rownames(expression), cell_ids)
  testthat::expect_equal(colnames(expression), colnames(counts))

  if (!is.null(feature_info)) {
    testthat::expect_is(feature_info, "data.frame")
    testthat::expect_equal(colnames(counts), feature_info$feature_id)
    testthat::expect_equal(colnames(expression), feature_info$feature_id)
  } else {
    feature_info <- tibble(feature_id = colnames(counts))
  }

  # create output structure
  model %>% extend_with(
    "dynwrap::with_expression",
    counts = counts,
    expression = expression,
    feature_info = feature_info,
    ...
  )
}

#' @rdname add_expression
#' @export
is_wrapper_with_expression <- function(model) {
  is_data_wrapper(model) && "dynwrap::with_expression" %in% class(model)
}

#' @rdname add_expression
#' @export
get_expression <- function(model, expression_source = "expression") {
  if (is.character(expression_source)) {
    if(!expression_source %in% names(model)) {stop(glue::glue("No expression found in trajectory, please provide the expression through the {crayon::italic('expression_source')} argument. This can be an expression or counts matrix, or a dataset containing the expression."))}
    expression <- model[[expression_source]]
  } else if (is.matrix(expression_source)) {
    expression <- expression_source
  } else if (is_wrapper_with_expression(expression_source)) {
    expression <- get_expression(expression_source)
  } else if (is.function(expression_source)) {
    expression <- expression_source
  } else {
    stop("Invalid expression_source")
  }

  if (is.function(expression)) {
    expression <- expression()
  }

  expression
}

#' Create a wrapper object with expression and counts
#'
#' @inheritParams add_expression
#' @inheritParams wrap_data
#'
#' @export
wrap_expression <- function(
  id = NULL,
  expression,
  counts,
  cell_info = NULL,
  feature_info = NULL,
  ...
) {
  testthat::expect_equivalent(dim(expression), dim(counts))
  testthat::expect_equivalent(colnames(expression), colnames(counts))
  testthat::expect_equivalent(rownames(expression), rownames(counts))

  wrap_data(
    id = id,
    cell_ids = rownames(expression),
    cell_info = cell_info,
    ...
  ) %>%
    add_expression(
      counts = counts,
      expression = expression,
      feature_info = feature_info
    )
}
