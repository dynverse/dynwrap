#' Label milestones either manually (`label_milestones`) or using marker genes (`label_milestones_markers`)
#'
#' `label_milestones` can be used to manually assign labels to a milestone using their identifiers
#'
#' `label_milestones_markers` will assign a label to a milestone if its marker profile most closely resembles a given profile
#'
#' @inheritParams common_param
#' @param labelling Named character vector containing for a milestone a new label
#' @param markers List containing for each label a list of marker genes
#' @param expression_source The expression source
#' @param n_nearest_cells The number of nearest cells to use for extracting milestone expression
#' @param label_milestones How to label the milestones. Can be TRUE (in which case the labels within the trajectory will be used), "all" (in which case both given labels and milestone_ids will be used), a named character vector, or FALSE
#'
#' @return
#' **`label_milestones`**:  A trajectory object with *milestone_labelling*, a named vector where milestone identifiers are mapped to their labels
#'
#' **`get_milestone_labelling`**: A named vector giving a mapping between milestones and their labels. If certain milestones were not given a label, this vector will give the identifiers themselves.
#'
#' @keywords adapt_trajectory
#'
#' @examples
#' trajectory <- example_trajectory
#'
#' # manual labelling
#' trajectory <- label_milestones(
#'   trajectory,
#'   labelling = c("milestone_begin" = "Let's go")
#' )
#' get_milestone_labelling(trajectory)
#'
#' # marker gene labelling
#' trajectory <- label_milestones_markers(
#'   trajectory,
#'   markers = list(A_high = "A")
#' )
#' get_milestone_labelling(trajectory)
#'
#' is_wrapper_with_milestone_labelling(trajectory)
#'
#' @export
label_milestones <- function(trajectory, labelling) {
  milestone_ids <- trajectory$milestone_ids

  assert_that(is.character(labelling))
  assert_that(length(names(labelling)) == length(labelling))
  assert_that(names(labelling) %all_in% milestone_ids)

  # now overwrite the existing labelling if present
  if (!is.null(trajectory$milestone_labelling)) {
    milestone_labelling <- trajectory$milestone_labelling
  } else {
    milestone_labelling <- set_names(rep(NA, length(milestone_ids)), milestone_ids)
  }

  milestone_labelling[names(labelling)] <- labelling

  # add labelling to wrapper
  trajectory$milestone_labelling <- milestone_labelling

  trajectory %>% extend_with(
    "dynwrap::with_milestone_labelling",
    milestone_labelling = milestone_labelling
  )
}

#' @rdname label_milestones
#' @export
label_milestones_markers <- function(trajectory, markers, expression_source = "expression", n_nearest_cells = 20) {
  assert_that(length(markers) > 0)

  milestone_ids <- trajectory$milestone_ids
  expression <- get_expression(trajectory, expression_source)

  local_expression <- map2_df(names(markers), markers, function(new_milestone_id, features_oi) {
    map_df(milestone_ids, function(milestone_id) {
      assert_that(all_in(features_oi, colnames(expression)))

      cells_oi <- trajectory$milestone_percentages %>%
        filter(milestone_id == !!milestone_id) %>%
        top_n(n_nearest_cells, percentage) %>%
        pull(cell_id)

      tibble(
        milestone_id = milestone_id,
        new_milestone_id = new_milestone_id,
        expression = mean(expression[cells_oi, features_oi])
      )
    }) %>%
      mutate(new_milestone_id = new_milestone_id)
  })

  # select top old milestone id
  mapping <- local_expression %>%
    group_by(new_milestone_id) %>%
    top_n(1, expression) %>%
    ungroup()

  # multiple mappings
  if (any(table(mapping$new_milestone_id) > 1)) {
    too_many <- table(mapping$new_milestone_id) %>% keep(~. > 1) %>% names()
    warning(stringr::str_glue("{too_many} was mapped to multiple milestones, adding integer suffices"))

    mapping <- mapping %>%
      group_by(new_milestone_id) %>%
      mutate(
        new_new_milestone_id = ifelse(n() > 1, new_milestone_id, paste0(new_milestone_id, "_", row_number()))
      ) %>%
      ungroup() %>%
      select(new_milestone_id = new_new_milestone_id)
  }

  # now overwrite the existing labelling if present
  if (!is.null(trajectory$milestone_labelling)) {
    milestone_labelling <- trajectory$milestone_labelling
  } else {
    milestone_labelling <- set_names(rep(NA, length(milestone_ids)), milestone_ids)
  }

  milestone_labelling[mapping$milestone_id] <- mapping$new_milestone_id

  # add labelling to wrapper
  trajectory$milestone_labelling <- milestone_labelling

  trajectory %>% extend_with(
    "dynwrap::with_milestone_labelling",
    milestone_labelling = milestone_labelling
  )
}



#' @rdname label_milestones
#' @export
is_wrapper_with_milestone_labelling <- function(trajectory) {
  is_wrapper_with_trajectory(trajectory) && "dynwrap::with_milestone_labelling" %in% class(trajectory)
}

#' @rdname label_milestones
#' @export
get_milestone_labelling <- function(trajectory, label_milestones = NULL) {
  if(is.character(label_milestones) && length(names(label_milestones)) == length(label_milestones)) {
    assert_that(names(label_milestones) %all_in% trajectory$milestone_ids)
    labels <- label_milestones
  } else if (is.null(label_milestones) || label_milestones == TRUE) {
    if (is_wrapper_with_milestone_labelling(trajectory)) {
      labels <- trajectory$milestone_labelling
    } else {
      labels <- set_names(trajectory$milestone_ids, trajectory$milestone_ids)
    }
  } else if (label_milestones == "all") {
    labels <- set_names(trajectory$milestone_ids, trajectory$milestone_ids)
    if (is_wrapper_with_milestone_labelling(trajectory)) {
      labels <- c(labels[names(trajectory$milestone_labelling)[is.na(trajectory$milestone_labelling)]], trajectory$milestone_labelling)
    }
  } else  {
    labels <- character()
  }

  labels[setdiff(trajectory$milestone_ids, names(labels))] <- NA

  labels
}
