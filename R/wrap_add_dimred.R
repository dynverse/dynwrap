#' Add or create a dimensionality reduction
#'
#' @inheritParams common_param
#' @param dimred The dimensionality reduction matrix (with cell_ids as rownames) or function which will run the dimensionality reduction
#' @param dimred_milestones An optional dimensionality reduction of the milestones.
#' @param dimred_segment_progressions An optional progression matrix of the trajectory segments. Format: `tibble(from, to, percentage)`
#' @param dimred_segment_points An optional dimensionality reduction of the trajectory segments. Format: `matrix(Comp1, Comp2, ...)`.
#' @param connect_segments Whether to connect segments between edges
#' @param ... extra information to be stored in the wrapper
#'
#' @keywords adapt_trajectory
#'
#' @inheritParams get_expression
#'
#' @export
add_dimred <- function(
  dataset,
  dimred,
  dimred_milestones = NULL,
  dimred_segment_progressions = NULL,
  dimred_segment_points = NULL,
  connect_segments = FALSE,
  expression_source = "expression",
  ...
) {
  testthat::expect_true(is_data_wrapper(dataset))

  # run or process dimred
  cell_ids <- dataset$cell_ids
  if (is.matrix(dimred) || is.data.frame(dimred)) {
    dimred <- process_dimred(dataset, dimred)
    testthat::expect_setequal(dataset$cell_id, rownames(dimred))
  } else {
    dimred <- get_dimred(dataset, dimred, expression_source)
  }

  if (!is.null(dimred_milestones)) {
    dimred_milestones <- process_dimred(dataset, dimred_milestones, "milestone_id")
    if (is_wrapper_with_trajectory(dataset)) {
      milestone_ids <- dataset$milestone_ids
      dimred_milestones <- dimred_milestones[milestone_ids, ]

      assert_that(identical(rownames(dimred_milestones), milestone_ids))
    }
  }

  if (!is.null(dimred_segment_points) || !is.null(dimred_segment_progressions)) {
    dimred_segment_points <- process_dimred(dataset, dimred_segment_points, "segment_point_id", has_rownames = FALSE)
    assert_that(
      is.matrix(dimred_segment_points),
      is.data.frame(dimred_segment_progressions),
      identical(colnames(dimred_segment_points), colnames(dimred)),
      identical(colnames(dimred_segment_progressions), c("from", "to", "percentage")),
      nrow(dimred_segment_points) == nrow(dimred_segment_progressions)
    )
  }

  # TODO: add tests for connecting segments!
  if (isTRUE(connect_segments)) {
    connected <- connect_dimred_segments(
      dimred_segment_progressions,
      dimred_segment_points,
      dataset$milestone_network
    )
    dimred_segment_progressions <- connected$dimred_segment_progressions
    dimred_segment_points <- connected$dimred_segment_points
  }

  # create output structure
  dataset %>% extend_with(
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
is_wrapper_with_dimred <- function(dataset) {
  is_data_wrapper(dataset) && "dynwrap::with_dimred" %in% class(dataset)
}

#' @rdname add_dimred
#' @export
get_dimred <- function(dataset, dimred = NULL, expression_source = "expression") {
  if(is.function(dimred)) {
    # function -> calculate dimensionality reduction
    expression <- get_expression(dataset, expression_source)
    dimred <- dimred(expression)
  } else if (is.matrix(dimred)) {
    # matrix
    assert_that(is.numeric(dimred))
    assert_that(length(rownames(dimred)) == nrow(dimred))
    assert_that(dataset$cell_ids %all_in% rownames(dimred))
    assert_that(rownames(dimred) %all_in% dataset$cell_ids)

    colnames(dimred) <- paste0("comp_", seq_len(ncol(dimred)))
  } else if (is.data.frame(dimred)) {
    # dataframe
    if ("cell_id" %in% colnames(dimred)) {
      rownames(dimred) <- NULL
      dimred <- dimred %>%
        as.data.frame() %>%
        column_to_rownames("cell_id") %>%
        as.matrix()
    }
    assert_that(is.numeric(dimred))
    assert_that(length(rownames(dimred)) == nrow(dimred))
    assert_that(dataset$cell_ids %all_in% rownames(dimred))
    assert_that(rownames(dimred) %all_in% dataset$cell_ids)

    colnames(dimred) <- paste0("comp_", seq_len(ncol(dimred)))
  } else if (is_wrapper_with_dimred(dataset)) {
    # dimred within wrapper
    if (is.list(dataset$dimred)) {
      assert_that(dimred %all_in% names(dataset$dimred))
      dimred <- dataset$dimred[[dimred]]
    } else {
      dimred <- dataset$dimred
    }
    colnames(dimred) <- paste0("comp_", seq_len(ncol(dimred)))
  } else {
    stop("Invalid dimred argument")
  }

  # make sure the rownames are in the correct order
  dimred <- dimred[dataset$cell_ids, ]

  dimred
}


process_dimred <- function(dataset, dimred, identifier = "cell_id", has_rownames = TRUE) {
  if (is.matrix(dimred)) {
    # matrix
    assert_that(is.numeric(dimred))
    if (has_rownames) assert_that(length(rownames(dimred)) == nrow(dimred))

    colnames(dimred) <- paste0("comp_", seq_len(ncol(dimred)))
  } else if (is.data.frame(dimred)) {
    # dataframe
    if (has_rownames) {
      if (identifier %in% colnames(dimred)) {
        dimred[[identifier]] <- as.character(dimred[[identifier]])
        rownames(dimred) <- NULL
        dimred <-
          dimred %>%
          as.data.frame() %>%
          column_to_rownames(identifier) %>%
          as.matrix()
      }
      assert_that(length(rownames(dimred)) == nrow(dimred))
    } else {
      dimred <- dimred %>% as.matrix()
    }

    assert_that(is.numeric(dimred))

    colnames(dimred) <- paste0("comp_", seq_len(ncol(dimred)))
  }

  dimred
}





connect_dimred_segments <- function(dimred_segment_progressions, dimred_segment_points, milestone_network) {
  milestone_ids <- unique(c(milestone_network$from, milestone_network$to))
  connections <- milestone_ids %>% map(function(milestone_id) {
    # find the indices of the segment points that are closest to the milestone
    ixs <- dimred_segment_progressions %>%
      mutate(ix = row_number()) %>%
      group_by(from, to) %>%
      arrange(percentage) %>%
      filter(
        xor(
          (row_number() == 1) & (from == !!milestone_id),
          (row_number() == n()) & (to == !!milestone_id)
        )
      ) %>%
      pull(ix)

    if (length(ixs) > 0) {
      # we'll create a new point for each edge in the milestone network that contains this milestone

      # create progressions for each new point
      progressions <- bind_rows(
        milestone_network %>% filter(from == !!milestone_id) %>% select(from, to) %>% mutate(percentage = 0),
        milestone_network %>% filter(to == !!milestone_id) %>% select(from, to) %>% mutate(percentage = 1)
      )

      points <- dimred_segment_points[ixs, , drop = FALSE] %>% colMeans() %>% rep(nrow(progressions)) %>% matrix(nrow = nrow(progressions), byrow = TRUE)

      list(
        progressions = progressions,
        points = points
      )
    } else {
      list(
        progressions = tibble(from = character(), to = character(), percentage = numeric()),
        points = dimred_segment_points[0, ]
      )
    }
  })

  connecting_progressions <- connections %>% map_dfr("progressions")
  connecting_points <- connections %>% map("points") %>% do.call(rbind, .)

  list(
    dimred_segment_progressions = bind_rows(dimred_segment_progressions, connecting_progressions),
    connecting_points = rbind(dimred_segment_points, connecting_points)
  )
}




dimred_merged <- function(dimred, expression, expression_projected) {
  merge_projected(
    expression,
    expression_projected
  ) %>%
    dimred() %>%
    split_projected()
}




merge_projected <- function(expression, expression_projected) {
  rownames(expression_projected) <- paste0(rownames(expression_projected), "###PROJECTED")
  rbind(
    expression,
    expression_projected
  )
}
split_projected <- function(merged, cell_ids = str_subset(rownames(merged), ".*###PROJECTED", negate = TRUE)) {
  projected <- merged[paste0(cell_ids, "###PROJECTED"),]
  rownames(projected) <- cell_ids
  colnames(projected) <- paste0(colnames(projected), "_projected")

  lst(
    current = merged[cell_ids, ],
    projected
  )
  cbind(merged[cell_ids, ], projected)
}
