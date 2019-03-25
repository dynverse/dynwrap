#' Add count and normalised expression values to a dataset
#'
#' @inheritParams dynwrap
#' @param counts The counts with genes in columns and cells in rows
#' @param expression The normalised expression values with genes in columns and cells in rows
#' @param feature_info Optional meta-information of the features, a data.frame with at least feature_id as column
#' @param ... extra information to be stored in the dataset
#' @param expression_source The source of expression, can be "counts", "expression", an expression matrix, or another dataset which contains expression
#'
#' @keywords adapt_trajectory
#'
#' @export
#'
#' @importFrom Matrix Matrix
#' @importFrom testthat expect_equal expect_is
add_expression <- function(
  dataset,
  counts,
  expression,
  feature_info = NULL,
  ...
) {
  testthat::expect_true(is_data_wrapper(dataset))

  assert_that(!(is.null(counts) && is.null(expression)), msg = "counts and expression can't both be NULL")

  if (!is.null(counts)) {
    if (is.matrix(counts)) {
      counts <- Matrix::Matrix(counts, sparse = TRUE)
    }
    if (is_sparse(counts) && !"dgCMatrix" %in% class(counts)) {
      counts <- as(counts, "dgCMatrix")
    }
    assert_that(
      "dgCMatrix" %in% class(counts),
      identical(rownames(counts), dataset$cell_ids)
    )
  }

  if (!is.null(expression)) {
    if (is.matrix(expression)) {
      expression <- Matrix::Matrix(expression, sparse = TRUE)
    }
    assert_that(
      "dgCMatrix" %in% class(expression),
      identical(rownames(expression), dataset$cell_ids)
    )
  }

  if (!is.null(feature_info)) {
    assert_that(
      is.data.frame(feature_info),
      "feature_id" %all_in% colnames(feature_info),
      colnames(expression) %all_in% feature_info$feature_id,
      colnames(counts) %all_in% feature_info$feature_id
    )
  } else {
    feature_info <- tibble(feature_id = colnames(counts) %||% colnames(expression))
  }

  # create output structure
  dataset %>% extend_with(
    "dynwrap::with_expression",
    counts = counts,
    expression = expression,
    feature_info = feature_info,
    ...
  )
}

#' @rdname add_expression
#' @export
is_wrapper_with_expression <- function(dataset) {
  is_data_wrapper(dataset) && "dynwrap::with_expression" %in% class(dataset)
}

#' @rdname add_expression
#' @export
get_expression <- function(dataset, expression_source = "expression") {
  if (is.character(expression_source)) {
    if(!expression_source %in% names(dataset)) {
      stop(glue::glue(
        "No expression found in trajectory, please provide the expression through the {crayon::italic('expression_source')} argument. ",
        "This can be an expression or counts matrix, or a dataset containing the expression."
      ))
    }
    expression <- dataset[[expression_source]]
  } else if (is.matrix(expression_source) || dynutils::is_sparse(expression_source)) {
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
#' @keywords infer_trajectory
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
  cell_ids <- rownames(expression) %||% rownames(counts)
  feature_ids <- colnames(expression %||% colnames(counts))

  assert_that(!is.null(cell_ids))
  assert_that(!is.null(feature_ids))

  wrap_data(
    id = id,
    cell_ids = cell_ids,
    cell_info = cell_info,
    ...
  ) %>%
    add_expression(
      counts = counts,
      expression = expression,
      feature_ids = feature_ids,
      feature_info = feature_info
    )
}
