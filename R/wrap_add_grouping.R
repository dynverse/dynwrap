#' Add a cell grouping to a dataset
#'
#' @inheritParams common_param
#' @param grouping A grouping of the cells, can be a named vector or a dataframe with *group_id* and *cell_id*
#' @param group_ids All group identifiers, optional
#' @param ... Extra information to be stored in the dataset
#'
#' @keywords adapt_trajectory
#' 
#' @return A dynwrap object with the grouping added.
#'
#' @examples
#' dataset <- example_dataset
#'
#' grouping <- sample(c("A", "B", "C"), length(dataset$cell_ids), replace = TRUE)
#' names(grouping) <- dataset$cell_ids
#'
#' dataset <- add_grouping(dataset, grouping)
#' head(dataset$grouping)
#'
#' @export
add_grouping <- function(
  dataset,
  grouping,
  group_ids = NULL,
  ...
) {
  # process the grouping
  grouping <- process_grouping(dataset, grouping)

  # if grouping not provided, have to calculate group_ids here
  if(is.null(group_ids)) group_ids <- unique(grouping)

  # assume grouping is in the same order
  if(length(names(grouping)) != length(grouping)) {
    names(grouping) <- dataset$cell_ids
  }

  assert_that(
    # check whether dataset is a data wrapper
    is_data_wrapper(dataset),

    # check group ids
    is.character(group_ids),
    !any(duplicated(group_ids)),

    # check cell group
    !is.null(names(grouping)),
    is.character(grouping),
    names(grouping) %all_in% dataset$cell_ids,
    grouping[!is.na(grouping)] %all_in% group_ids
  )

  # create output structure
  dataset %>% extend_with(
    "dynwrap::with_grouping",
    group_ids = group_ids,
    grouping = grouping,
    ...
  )
}

#' @rdname add_grouping
#' @export
is_wrapper_with_grouping <- function(dataset) {
  is_data_wrapper(dataset) && "dynwrap::with_grouping" %in% class(dataset)
}

#' @rdname add_grouping
#' @export
get_grouping <- function(dataset, grouping = NULL) {
  if(is.null(grouping)) {
    # no grouping provided, get from dataset
    if(is_wrapper_with_grouping(dataset)) {
      grouping <- set_names(dataset$grouping, dataset$cell_ids)
    } else if (is_wrapper_with_prior_information(dataset)) {
      if("groups_id" %in% names(dataset$prior_information)) {
        grouping <- dataset$prior_information$groups_id %>%
          {set_names(.$group_id, .$cell_id)}
      }
    } else {
      stop("Wrapper does not contain a grouping, provide grouping or add a grouping to wrapper using add_grouping")
    }
  }  else if (length(grouping) == 1 && is.character(grouping)) {
    # extract group from column in cell_info
    if(grouping %in% colnames(dataset$cell_info)) {
      grouping <- set_names(dataset$cell_info[[grouping]], dataset$cell_id)
    } else {
      stop("Could not find column ", grouping, " in cell_info")
    }
  } else {
    grouping <- process_grouping(dataset, grouping)
  }

  if(length(names(grouping)) != length(grouping)) {
    names(grouping) <- dataset$cell_ids
  }

  grouping
}


process_grouping <- function(dataset, grouping) {
  if (is.data.frame(grouping) && all(c("group_id", "cell_id") %in% colnames(grouping))) {
    # dataframe
    grouping <- set_names(as.character(grouping$group_id), grouping$cell_id)
  } else if (length(grouping) == length(dataset$cell_ids)) {
    # named vector of all cells
    if (is.null(names(grouping))) {
      names(grouping) <- dataset$cell_ids
    }
  } else if (length(grouping) == length(names(grouping))) {
    # named vector, possibly not containing all cells
  } else {
    stop("Could not find grouping")
  }

  # cells which are not grouped are given group NA
  grouping[setdiff(dataset$cell_ids, names(grouping))] <- NA

  # make sure the order of the grouping is the same as cell_ids
  grouping <- grouping[dataset$cell_ids]

  set_names(as.character(grouping), dataset$cell_ids)
}


#' Create a grouping from a trajectory
#'
#' Grouping cells onto their edges, or grouping cells onto their nearest milestones
#'
#' @inheritParams common_param
#' @param group_template Processed by glue::glue to name the group
#'
#' @name group_from_trajectory
#'
#' @keywords derive_trajectory
NULL

#' @rdname group_from_trajectory
#' @export
group_onto_trajectory_edges <- function(trajectory, group_template = "{from}->{to}") {
  # first map cells to largest percentage (in case of divergence regions)
  progressions <-
    trajectory$progressions %>%
    group_by(cell_id) %>%
    arrange(-percentage) %>%
    slice(1) %>%
    ungroup()

  # do the actual grouping
  grouping <-
    progressions %>%
    group_by(from, to) %>%
    mutate(group_id = as.character(glue::glue(group_template))) %>%
    ungroup() %>%
    select(cell_id, group_id) %>%
    deframe()

  cell_ids <- trajectory$cell_ids
  ifelse(cell_ids %in% names(grouping), grouping[cell_ids], NA) %>%
    set_names(cell_ids)
}


#' @rdname group_from_trajectory
#' @export
group_onto_nearest_milestones <- function(trajectory) {
  grouping <- trajectory$milestone_percentages %>%
    group_by(cell_id) %>%
    arrange(-percentage) %>%
    slice(1) %>%
    mutate(percentage = 1) %>%
    ungroup() %>%
    select(cell_id, milestone_id) %>%
    deframe()

  cell_ids <- trajectory$cell_ids
  ifelse(cell_ids %in% names(grouping), grouping[cell_ids], NA) %>%
    set_names(cell_ids)
}

