#' Add a cell grouping to a data wrapper
#'
#' @param model The model to which the grouping will be added.
#' @param grouping A grouping of the cells, can be a named vector or a dataframe with group_id (character) and cell_id (character)
#' @param group_ids All group_ids, optional
#' @param ... Extra information to be stored in the wrapper.
#'
#' @export
#'
#' @importFrom testthat expect_equal expect_is expect_true
add_grouping <- function(
  model,
  grouping,
  group_ids = NULL,
  ...
) {
  # process the grouping
  grouping <- process_grouping(model, grouping)

  # if grouping not provided, have to calculate group_ids here
  if(is.null(group_ids)) group_ids <- unique(grouping)

  #
  if(length(names(grouping)) != length(grouping)) {
    names(grouping) <- model$cell_ids
  }

  # check whether object is a data wrapper
  testthat::expect_true(is_data_wrapper(model))

  # check group ids
  testthat::expect_is(group_ids, "character")
  testthat::expect_false(any(duplicated(group_ids)))

  # check cell group
  testthat::expect_named(grouping)
  testthat::expect_is(grouping, "character")
  testthat::expect_true(all(names(grouping) %in% model$cell_ids))
  testthat::expect_true(all(grouping %in% group_ids))

  # check milestone ids, if data contains a trajectory
  if (is_wrapper_with_trajectory(model)) {
    testthat::expect_equal(model$milestone_ids, group_ids)
  }

  # create output structure
  model %>% extend_with(
    "dynwrap::with_grouping",
    group_ids = group_ids,
    grouping = grouping,
    ...
  )
}

#' @rdname add_grouping
#' @export
is_wrapper_with_grouping <- function(model) {
  is_data_wrapper(model) && "dynwrap::with_grouping" %in% class(model)
}

#' @rdname add_grouping
#' @export
get_grouping <- function(model, grouping = NULL) {
  if(is.null(grouping)) {
    # no grouping provided, get from model
    if(is_wrapper_with_grouping(model)) {
      grouping <- set_names(model$grouping, model$cell_ids)
    } else if (is_wrapper_with_prior_information(model)) {
      if("groups_id" %in% names(model$prior_information)) {
        grouping <- model$prior_information$groups_id %>%
          {set_names(.$group_id, .$cell_id)}
      }
    } else {
      stop("Wrapper does not contain a grouping, provide grouping or add a grouping to wrapper using add_grouping")
    }
  }  else if (length(grouping) == 1 && is.character(grouping)) {
    # extract group from column in cell_info
    if(grouping %in% colnames(model$cell_info)) {
      grouping <- set_names(model$cell_info[[grouping]], model$cell_id)
    } else {
      stop("Could not find column ", grouping, " in cell_info")
    }
  } else {
    grouping <- process_grouping(model, grouping)
  }

  if(length(names(grouping)) != length(grouping)) {
    names(grouping) <- model$cell_ids
  }

  grouping
}


process_grouping <- function(model, grouping) {
  if (is.data.frame(grouping) && all(c("group_id", "cell_id") %in% colnames(grouping))) {
    # dataframe
    grouping <- set_names(as.character(grouping$group_id), grouping$cell_id)
  } else if (length(grouping) == length(model$cell_ids)) {
    # named vector of all cells
  } else if (length(grouping) == length(names(grouping))) {
    # named vector not containing all cells
    grouping[setdiff(names(grouping), model$cell_id)] <- NA
  } else {
    stop("Could not find grouping")
  }

  # cells which are not grouped are given group NA
  grouping[setdiff(model$cell_ids, names(grouping))] <- NA

  grouping
}
