#' Add a cell grouping to a data wrapper
#'
#' @param data_wrapper A data wrapper to extend upon.
#' @param group_ids All group_ids, optional
#' @param grouping A grouping of the cells, can be
#' @param ... Extra information to be stored in the wrapper.
#'
#' @export
#'
#' @importFrom testthat expect_equal expect_is expect_true
add_grouping <- function(
  data_wrapper,
  group_ids,
  grouping = NULL,
  ...
) {
  # if grouping not provided, will use group_ids
  if (is.null(grouping)) {
    grouping <- group_ids
    group_ids <- NULL
  }

  # process the grouping
  grouping <- get_grouping(data_wrapper, grouping)

  # if grouping not provided, have to calculate group_ids here
  if(is.null(group_ids)) group_ids <- unique(grouping)

  #
  if(length(names(grouping)) != length(grouping)) {
    names(grouping) <- data_wrapper$cell_ids
  }

  # check whether object is a data wrapper
  testthat::expect_true(is_data_wrapper(data_wrapper))

  # check group ids
  testthat::expect_is(group_ids, "character")
  testthat::expect_false(any(duplicated(group_ids)))

  # check cell group
  testthat::expect_named(grouping)
  testthat::expect_is(grouping, "character")
  testthat::expect_true(all(names(grouping) %in% data_wrapper$cell_ids))
  testthat::expect_true(all(grouping %in% group_ids))

  # check milestone ids, if data contains a trajectory
  if (is_wrapper_with_trajectory(data_wrapper)) {
    testthat::expect_equal(data_wrapper$milestone_ids, group_ids)
  }

  # create output structure
  data_wrapper %>% extend_with(
    "dynwrap::with_grouping",
    group_ids = group_ids,
    grouping = grouping,
    ...
  )
}

#' @rdname add_grouping
#' @export
is_wrapper_with_grouping <- function(data_wrapper) {
  is_data_wrapper(data_wrapper) && "dynwrap::with_grouping" %in% class(data_wrapper)
}

#' @rdname add_grouping
#' @export
get_grouping <- function(data_wrapper, grouping = NULL) {
  if(is.null(grouping)) {
    # no grouping provided, use
    if(is_wrapper_with_grouping(data_wrapper)) {
      grouping <- set_names(data_wrapper$grouping, data_wrapper$cell_ids)
    } else if (is_wrapper_with_prior_information(data_wrapper)) {
      if("grouping_assignment" %in% names(data_wrapper$prior_information)) {
        grouping <- data_wrapper$prior_information$grouping_assignment %>%
          {set_names(.$group_id, .$cell_id)}
      }
    } else {
      stop("Wrapper does not contain a grouping, provide grouping or add a grouping to wrapper using add_grouping")
    }
  } else if (is.data.frame(grouping) && all(c("group_id", "cell_id") %in% colnames(grouping))) {
    grouping <- set_names(grouping$group_id, grouping$cell_id)
  } else if (length(grouping) == length(data_wrapper$cell_ids)) {
    grouping <- grouping
  } else if (length(grouping) == length(names(grouping))) {
    grouping <- grouping
    grouping[setdiff(names(grouping), data_wrapper$cell_id)] <- NA
  } else if (length(grouping) == 1 && is.character(grouping)) {
    # column in cell_info
    if(grouping %in% colnames(data_wrapper$cell_info)) {
      grouping <- set_names(data_wrapper$cell_info[[grouping]], data_wrapper$cell_id)
    } else {
      stop("Could not find column ", grouping, " in cell_info")
    }
  } else {
    stop("Could not find grouping")
  }

  if(length(names(grouping)) != length(grouping)) {
    names(grouping) <- data_wrapper$cell_ids
  }

  grouping
}
