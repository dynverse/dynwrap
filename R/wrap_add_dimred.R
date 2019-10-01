#' Add or create a dimensionality reduction
#'
#' This can also perform dimensionality reduction of
#' - The projected expression state with RNA velocity, only if `dimred` is a function and  `pair_with_velocity=TRUE`
#' - The trajectory, by projecting the milestones and some "waypoints" to the reduced space, only if `dataset` contains a trajectory
#'
#' @inheritParams common_param
#' @inheritParams get_expression
#' @param dimred Can be
#' - A function which will perform the dimensionality reduction, see [`dyndimred::list_dimred_methods()`][dyndimred::dimred()]
#' - A matrix with the dimensionality reduction, with cells in rows and dimensions (*comp_1*, *comp_2*, ...) in columns
#' @param dimred_projected An optional dimensionality reduction of the projected cells (RNA velocity). A matrix with cells in rows and components (*comp_1*, *comp_2*, ...) in columns
#' @param dimred_milestones An optional dimensionality reduction of the milestones. A matrix with milestones in rows and components (*comp_1*, *comp_2*, ...) in columns
#'
#' This will be automatically calculated if `project_trajectory = TRUE`
#' @param dimred_segment_progressions,dimred_segment_points An optional set of points along the trajectory with their dimensionality reduction. `dimred_segment_progressions` is a dataframe containing the *from* and *to* milestones, and their *progression*. `dimred_segment_points` is a matrix with points (the same number as in `dimred_segment_progressions`) in rows and components (*comp_1*, *comp_2*, ...) in columns. Both objects have the same number of rows.
#'
#' These will be automatically calculated if `project_trajectory = TRUE`
#' @param project_trajectory Whether to also project the trajectory. Only relevant if dataset contains a trajectory, and dimred_segment_progressions and dimred_segment_points are not provided
#' @param connect_segments Whether to connect segments between edges
#' @param pair_with_velocity Whether to generate a paired dimensionality reduction with the projected expression.
#' @param ... extra information to be stored in the wrapper
#'
#' @return
#' A dataset object with *dimred*, which is a numeric matrix with cells in rows and the different components in columns.
#' - If the dataset contained projected expression, *dimred_projected* will also be present, which contains dimensionality reduction of the projected cells. It is a matrix with the locations of projected cells (rows) in the dimensions (columns)
#' - If the dataset contained a trajectory, and `project_trajectory=TRUE` (default), *dimred_milestones*, *dimred_segment_progressions* and *dimred_segment_points* will also be present. These are described in [project_trajectory()].
#'
#' @keywords adapt_trajectory
#'
#' @examples
#' dataset <- example_dataset
#' dataset <- add_dimred(
#'   dataset,
#'   dyndimred::dimred_landmark_mds
#' )
#' head(dataset$dimred)
#'
#' @seealso [dyndimred::list_dimred_methods()], [project_trajectory()]
#'
#' @export
add_dimred <- function(
  dataset,
  dimred,
  dimred_projected = NULL,
  dimred_milestones = NULL,
  dimred_segment_progressions = NULL,
  dimred_segment_points = NULL,
  project_trajectory = TRUE,
  connect_segments = FALSE,
  pair_with_velocity = !is.null(dataset$expression_projected),
  expression_source = "expression",
  ...
) {
  assert_that(is_data_wrapper(dataset))

  # run or process dimred
  cell_ids <- dataset$cell_ids
  if (is.matrix(dimred) || is.data.frame(dimred)) {
    dimred <- process_dimred(dataset, dimred)
    assert_that(
      rownames(dimred) %all_in% dataset$cell_id,
      dataset$cell_id %all_in% rownames(dimred)
    )

    if (!is.null(dimred_projected)) {
      assert_that(is.matrix(dimred_projected) || is.data.frame(dimred_projected))
      dimred_projected <- process_dimred(dataset, dimred_projected)
    }

  } else {
    # run dimred
    if (!pair_with_velocity) {
      expression <- get_expression(dataset, expression_source)
      dimred <- dimred(expression)
    } else {
      assert_that(expression_source == "expression", msg = "Can only do paired dimensionality reduction with velocity if expression source is expression")
      expression <- get_expression(dataset, "expression")
      expression_projected <- get_expression(dataset, "expression_projected")
      out <- dimred_merged(dimred, expression = expression, expression_projected = expression_projected)
      dimred <- out$current
      dimred_projected <- out$projected
    }

  }

  if (!is.null(dimred_milestones)) {
    dimred_milestones <- process_dimred(dataset, dimred_milestones, "milestone_id")
    if (is_wrapper_with_trajectory(dataset)) {
      milestone_ids <- dataset$milestone_ids
      dimred_milestones <- dimred_milestones[milestone_ids, ]

      assert_that(identical(rownames(dimred_milestones), milestone_ids))
    }
  } else if (is_wrapper_with_trajectory(dataset) && project_trajectory) {
    dimred_milestones <- project_milestones(dataset, dimred)
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
  } else if (is_wrapper_with_trajectory(dataset) && project_trajectory) {
    projection <- project_trajectory(dataset, dimred)
    dimred_segment_progressions <- projection$dimred_segment_progressions
    dimred_segment_points <- projection$dimred_segment_points
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
    dimred_projected = dimred_projected,
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
#' @param return_other_dimreds Whether or not to return also the milestone dimreds and the segment dimreds, if available.
#' @export
get_dimred <- function(dataset, dimred = NULL, expression_source = "expression", return_other_dimreds = FALSE) {
  extra_out <- NULL

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

      if (return_other_dimreds) {
        extra_out <- list()

        if (!is.null(dataset$dimred_milestones)) {
          dimred_milestones <- dataset$dimred_milestones
          colnames(dimred_milestones) <-
            paste0("comp_", seq_len(ncol(dimred_milestones)))
          extra_out$dimred_milestones <- dimred_milestones
        }

        if (!is.null(dataset$dimred_segment_progressions) && !is.null(dataset$dimred_segment_points)) {
          dimred_segment_points <- dataset$dimred_segment_points
          dimred_segment_progressions <- dataset$dimred_segment_progressions
          colnames(dimred_segment_points) <-
            paste0("comp_", seq_len(ncol(dimred_segment_points)))
          extra_out$dimred_segment_points <- dimred_segment_points
          extra_out$dimred_segment_progressions <- dimred_segment_progressions
        }

        if (!is.null(dataset$dimred_projected)) {
          dimred_projected <- dataset$dimred_projected
          colnames(dimred_projected) <-
            paste0("comp_", seq_len(ncol(dimred_projected)))
          extra_out$dimred_projected <- dimred_projected
        }

      }
    }
    colnames(dimred) <- paste0("comp_", seq_len(ncol(dimred)))
  } else {
    stop("Invalid dimred argument")
  }

  # make sure the rownames are in the correct order
  dimred <- dimred[dataset$cell_ids, ]

  if (!is.null(extra_out)) {
    attr(dimred, "extra") <- extra_out
  }

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
  # often, not all features are used to calculate the projected expression
  # this is because enough spliced and unspliced reads need to be present
  # this means that there are often more features in expression than in expression_projected
  # we only use the common set of features here
  rownames(expression_projected) <- paste0(rownames(expression_projected), "###PROJECTED")
  feature_ids <- intersect(colnames(expression), colnames(expression_projected))
  if (length(feature_ids) == 0) {
    stop("No common features between expression and expression_projected")
  }

  rbind(
    expression[, feature_ids],
    expression_projected[, feature_ids]
  )
}
split_projected <- function(merged, cell_ids = str_subset(rownames(merged), ".*###PROJECTED", negate = TRUE)) {
  projected <- merged[paste0(cell_ids, "###PROJECTED"),]
  rownames(projected) <- cell_ids

  lst(
    current = merged[cell_ids, ],
    projected
  )
}
