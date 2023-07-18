#' Add count and normalised expression values to a dataset
#'
#' @inheritParams common_param
#' @param counts The counts values of genes (columns) within cells (rows). This can be both a dense and sparse matrix.
#' @param expression The normalised expression values of genes (columns) within cells (rows). This can be both a dense and sparse matrix.
#' @param expression_future Projected expression using RNA velocity of genes (columns) within cells (rows).  This can be both a dense and sparse matrix.
#' @param feature_info Optional meta-information of the features, a dataframe with at least *feature_id* as column
#' @param ... extra information to be stored in the dataset
#' @param expression_source The source of expression, can be "counts", "expression", an expression matrix, or another dataset which contains expression
#'
#' @keywords adapt_trajectory
#' 
#' @return A dynwrap object with the expression added.
#'
#' @examples
#' cell_ids <- c("A", "B", "C")
#' counts <- matrix(sample(0:10, 3*10, replace = TRUE), nrow = 3)
#' rownames(counts) <- cell_ids
#' colnames(counts) <- letters[1:10]
#' expression <- log2(counts + 1)
#'
#' dataset <- wrap_data(id = "my_awesome_dataset", cell_ids = cell_ids)
#' dataset <- add_expression(dataset, counts = counts, expression = expression)
#'
#' str(dataset$expression)
#' str(dataset$counts)
#'
#' @export
#'
#' @importFrom Matrix Matrix
add_expression <- function(
  dataset,
  counts,
  expression,
  feature_info = NULL,
  expression_future = NULL,
  ...
) {
  assert_that(is_data_wrapper(dataset))

  assert_that(!(is.null(counts) && is.null(expression)), msg = "counts and expression can't both be NULL")

  # convert expression if needed
  counts <- convert_expression(counts, dataset$cell_ids)
  expression <- convert_expression(expression, dataset$cell_ids)
  expression_future <- convert_expression(expression_future, dataset$cell_ids)

  if (is.null(feature_info) && !is.null(dataset$feature_info)) {
    feature_info <- dataset$feature_info
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
    expression_future = expression_future,
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
#' Projected expression based on RNA velocity can also be added to the wrapper through the `expression_future` argument
#'
#' Information about the cells and/or features can be added through `cell_info` and `feature_info`
#'
#' @inheritParams add_expression
#' @inheritParams wrap_data
#'
#' @keywords infer_trajectory
#'
#' @examples
#' dataset <- wrap_expression(
#'   counts = example_dataset$counts,
#'   expression = example_dataset$expression,
#'   expression_future = example_dataset$expression_future
#' )
#'
#' dataset$counts[1:10, 1:3]
#' dataset$expression[1:10, 1:3]
#' dataset$expression_future[1:10, 1:3]
#'
#' @export
wrap_expression <- function(
  id = NULL,
  expression,
  counts,
  cell_info = NULL,
  feature_info = NULL,
  expression_future = NULL,
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
    feature_ids = feature_ids,
    feature_info = feature_info,
    ...
  ) %>%
    add_expression(
      counts = counts,
      expression = expression,
      expression_future = expression_future
    )
}

#' @importFrom methods as
convert_expression <- function(x, cell_ids) {
  if (!is.null(x)) {
    if (is.matrix(x)) {
      x <- Matrix::Matrix(x, sparse = TRUE)
    }
    if (is_sparse(x) && !"dgCMatrix" %in% class(x)) {
      x <- as(x, "dgCMatrix")
    }
    assert_that(
      "dgCMatrix" %in% class(x),
      identical(rownames(x), cell_ids)
    )
    x
  }
  x
}
