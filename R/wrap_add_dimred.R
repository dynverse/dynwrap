#' Add a dimensionality reduction to a data wrapper
#'
#' TODO: add possibility to also dimred the milestones and segments. This should be migrated from dynplot!
#'
#' @param data_wrapper A data wrapper to extend upon.
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
  data_wrapper,
  dimred,
  dimred_milestones = NULL,
  dimred_trajectory_segments = NULL,
  expression_source = "expression",
  ...
) {
  testthat::expect_true(is_data_wrapper(data_wrapper))

  # run or process dimred
  if (!is.matrix(dimred)) {
    dimred <- get_dimred(data_wrapper, dimred, expression_source)
  }

  cell_ids <- data_wrapper$cell_ids

  testthat::expect_is(dimred, "matrix")
  testthat::expect_equal(rownames(dimred), cell_ids)

  if (!is.null(dimred_milestones)) {
    testthat::expect_is(dimred_milestones, "matrix")
    testthat::expect_equal(colnames(dimred_milestones), colnames(dimred))

    if (is_wrapper_with_trajectory(data_wrapper)) {
      milestone_ids <- data_wrapper$milestone_ids
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
  data_wrapper %>% extend_with(
    "dynwrap::with_dimred",
    dimred = dimred,
    dimred_milestones = dimred_milestones,
    dimred_trajectory_segments = dimred_trajectory_segments,
    ...
  )
}

#' @rdname add_dimred
#' @export
is_wrapper_with_dimred <- function(data_wrapper) {
  is_data_wrapper(data_wrapper) && "dynwrap::with_dimred" %in% class(data_wrapper)
}

#' @rdname add_dimred
#' @export
get_dimred <- function(data_wrapper, dimred = NULL, expression_source = "expression") {
  if(is.function(dimred)) {
    # function
    expression <- get_expression(data_wrapper, expression_source)
    dimred <- dimred(expression)
  } else if (is.matrix(dimred)) {
    # matrix
    testthat::expect_true(length(rownames(dimred)) == nrow(dimred))
    testthat::expect_setequal(data_wrapper$cell_ids, rownames(dimred))

    colnames(dimred) <- paste0("comp_", seq_len(ncol(dimred)))
  } else if (is_wrapper_with_dimred(data_wrapper)) {
    # dimred within wrapper
    if(is.list(data_wrapper$dimred)) {
      testthat::expect_true(dimred %in% names(data_wrapper$dimred))
      dimred <- data_wrapper$dimred[[dimred]]
    } else {
      dimred <- data_wrapper$dimred
    }
    colnames(dimred) <- paste0("comp_", seq_len(ncol(dimred)))
  } else {
    stop("Invalid dimred argument")
  }

  dimred
}



# Process dimred from file ----------------------------------------

write_dimred_generic <- function(type = "cell", file = "dimred.csv") {
  function(dimred, dir_output) {
    dimred %>% as.data.frame() %>% rownames_to_column(paste0(type, "_id")) %>%
      write_csv(file.path(dir_output, file))
  }
}

read_dimred_generic <- function(type = "cell", file = "dimred.csv", check_exists = FALSE) {
  function(dir_output) {
    path <- file.path(dir_output, file)

    if(check_exists) {
      if(!file.exists(path)) {
        return(NULL)
      }
    }

    # change col_type to type + _id
    col_types <- cols(
      cell_id = col_character(),
      .default = col_double()
    )
    names(col_types$cols) <- paste0(type, "_id")

    dimred <- read_csv(
      path,
      col_types = col_types
    ) %>%
      as.data.frame() %>%
      column_to_rownames(paste0(type, "_id")) %>%
      as.matrix()

    colnames(dimred) <- paste0("comp_", seq_len(ncol(dimred)))

    dimred
  }
}


#' @rdname add_dimred
#' @param dir_output Output directory
#' @export
write_dimred <- write_dimred_generic()

read_dimred <- read_dimred_generic()

#' @rdname add_dimred
#' @export
write_dimred_milestones <- write_dimred_generic("milestone", "dimred_milestones.csv")

read_dimred_milestones <- read_dimred_generic("milestone", "dimred_milestones.csv")

read_dimred_milestones_required <- read_dimred_generic("milestone", "dimred_milestones.csv", TRUE)


process_dimred <- function(model, dir_output) {
  dimred <- read_dimred(dir_output)
  dimred_milestones <- read_dimred_milestones(dir_output)

  model <- model %>% add_dimred(dimred, dimred_milestones)
}

output_processors <- output_processors %>% add_row(
  id="dimred",
  processor=list(process_dimred),
  required_files=list(c("dimred.csv")),
  optional_files=list(c("dimred_milestones.csv")),
  required_output=list(c()),
  description="Adds the dimensionality reduction of each cell, and, optionally also the dimensionality reduction of every milestone. Any number of dimensions can be used.",
  creates_trajectory = FALSE
)
