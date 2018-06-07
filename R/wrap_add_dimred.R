#' Add or create a dimensionality reduction
#'
#' TODO: add possibility to also dimred the milestones and segments. This should be migrated from dynplot!
#'
#' @param model The model to which a dimensionality reduction will be added.
#' @param dimred The dimensionality reduction matrix (with cell_ids as rownames) or function which will run the dimensionality reduction
#' @param dimred_milestones An optional dimensionality reduction of the milestones.
#' @param dimred_trajectory_segments An optional dimensionality reduction of the trajectory segments.
#' @param ... extra information to be stored in the wrapper
#'
#' @inheritParams get_expression
#'
#' @export
#'
#' @importFrom testthat expect_equal expect_is expect_true
add_dimred <- function(
  model,
  dimred,
  dimred_milestones = NULL,
  dimred_trajectory_segments = NULL,
  expression_source = "expression",
  ...
) {
  testthat::expect_true(is_data_wrapper(model))

  # run or process dimred
  cell_ids <- model$cell_ids
  if (is.matrix(dimred) || is.data.frame(dimred)) {
    dimred <- process_dimred(model, dimred)
    testthat::expect_setequal(model$cell_id, rownames(dimred))
  } else {
    dimred <- get_dimred(model, dimred, expression_source)
  }

  if (!is.null(dimred_milestones)) {
    dimred_milestones <- process_dimred(model, dimred_milestones, "milestone_id")
    if (is_wrapper_with_trajectory(model)) {
      milestone_ids <- model$milestone_ids
      dimred_milestones <- dimred_milestones[milestone_ids, ]
      testthat::expect_equal(rownames(dimred_milestones), milestone_ids)
    }
  }

  if (!is.null(dimred_trajectory_segments)) {
    testthat::expect_is(dimred_trajectory_segments, "matrix")
    expected_colnames <- c(
      paste0("from_", colnames(dimred)),
      paste0("to_", colnames(dimred))
    )
    testthat::expect_equal(colnames(dimred_trajectory_segments), expected_colnames)
  }

  # create output structure
  model %>% extend_with(
    "dynwrap::with_dimred",
    dimred = dimred,
    dimred_milestones = dimred_milestones,
    dimred_trajectory_segments = dimred_trajectory_segments,
    ...
  )
}

#' @rdname add_dimred
#' @export
is_wrapper_with_dimred <- function(model) {
  is_data_wrapper(model) && "dynwrap::with_dimred" %in% class(model)
}

#' @rdname add_dimred
#' @export
get_dimred <- function(model, dimred = NULL, expression_source = "expression") {
  if(is.function(dimred)) {
    # function
    expression <- get_expression(model, expression_source)
    dimred <- dimred(expression)
  } else if (is.matrix(dimred)) {
    # matrix
    testthat::expect_true(is.numeric(dimred))
    testthat::expect_true(length(rownames(dimred)) == nrow(dimred))
    testthat::expect_setequal(model$cell_ids, rownames(dimred))

    colnames(dimred) <- paste0("comp_", seq_len(ncol(dimred)))
  } else if (is.data.frame(dimred)) {
    # dataframe
    if ("cell_id" %in% colnames(dimred)) {
      dimred <- dimred %>%
        as.data.frame() %>%
        column_to_rownames("cell_id") %>%
        as.matrix()
    }
    testthat::expect_true(is.numeric(dimred))
    testthat::expect_true(length(rownames(dimred)) == nrow(dimred))
    testthat::expect_setequal(model$cell_ids, rownames(dimred))

    colnames(dimred) <- paste0("comp_", seq_len(ncol(dimred)))
  } else if (is_wrapper_with_dimred(model)) {
    # dimred within wrapper
    if(is.list(model$dimred)) {
      testthat::expect_true(dimred %in% names(model$dimred))
      dimred <- model$dimred[[dimred]]
    } else {
      dimred <- model$dimred
    }
    colnames(dimred) <- paste0("comp_", seq_len(ncol(dimred)))
  } else {
    stop("Invalid dimred argument")
  }

  dimred
}


process_dimred <- function(model, dimred, identifier = "cell_id") {
  if (is.matrix(dimred)) {
    # matrix
    testthat::expect_true(is.numeric(dimred))
    testthat::expect_true(length(rownames(dimred)) == nrow(dimred))

    colnames(dimred) <- paste0("comp_", seq_len(ncol(dimred)))
  } else if (is.data.frame(dimred)) {
    # dataframe
    if (identifier %in% colnames(dimred)) {
      dimred[[identifier]] <- as.character(dimred[[identifier]])
      dimred <- dimred %>%
        as.data.frame() %>%
        column_to_rownames(identifier) %>%
        as.matrix()
    }
    testthat::expect_true(is.numeric(dimred))
    testthat::expect_true(length(rownames(dimred)) == nrow(dimred))

    colnames(dimred) <- paste0("comp_", seq_len(ncol(dimred)))
  }

  dimred
}
