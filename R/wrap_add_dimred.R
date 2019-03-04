#' Add or create a dimensionality reduction
#'
#' @param model The model to which a dimensionality reduction will be added.
#' @param dimred The dimensionality reduction matrix (with cell_ids as rownames) or function which will run the dimensionality reduction
#' @param dimred_milestones An optional dimensionality reduction of the milestones.
#' @param dimred_segment_progressions An optional progression matrix of the trajectory segments. Format: `tibble(from, to, percentage)`
#' @param dimred_segment_points An optional dimensionality reduction of the trajectory segments. Format: `matrix(Comp1, Comp2, ...)`.
#' @param ... extra information to be stored in the wrapper
#'
#' @inheritParams get_expression
#'
#' @export
add_dimred <- function(
  model,
  dimred,
  dimred_milestones = NULL,
  dimred_segment_progressions = NULL,
  dimred_segment_points = NULL,
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

      assert_that(identical(rownames(dimred_milestones), milestone_ids))
    }
  }

  if (!is.null(dimred_segment_points) || !is.null(dimred_segment_progressions)) {
    dimred_segment_points <- process_dimred(model, dimred_segment_points, "segment_point_id")
    assert_that(
      is.matrix(dimred_segment_points),
      is.data.frame(dimred_segment_progressions),
      identical(colnames(dimred_segment_points), colnames(dimred)),
      identical(colnames(dimred_segment_progressions), c("from", "to", "percentage"))
    )
  }

  # create output structure
  model %>% extend_with(
    "dynwrap::with_dimred",
    dimred = dimred,
    dimred_milestones = dimred_milestones,
    dimred_segment_progressions = dimred_segment_progressions,
    dimred_segment_points = dimred_segment_points,
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
    testthat::expect_true(all(model$cell_ids %in% rownames(dimred)))

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

  # make sure the rownames are in the correct order
  dimred <- dimred[model$cell_ids, ]

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
