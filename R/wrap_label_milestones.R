#' Label milestones based on highest expression of a set of markers
#'
#' @param trajectory The trajectory
#' @param labelling List containing for each new name of a milestone the genes which will be used as markers
#' @param expression_source The expression source
#' @param n_nearest_cells The number of nearest cells to use for extracting milestone expression
#' @param label_milestones How to label the milestones. Can be TRUE (in which case the labels within the trajectory will be used), "all" (in which case both given labels as milestone_ids will be used), a named character vector, or FALSE
#'
#' @export
label_milestones <- function(trajectory, labelling, expression_source = "expression", n_nearest_cells = 20) {
  expression <- get_expression(trajectory, expression_source)
  milestone_ids <- trajectory$milestone_ids

  local_expression <- map2_df(names(labelling), labelling, function(new_milestone_id, features_oi) {
    map_df(milestone_ids, function(milestone_id) {
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

  # do the actual renaming
  milestone_labelling <- set_names(rep(NA, length(milestone_ids)), milestone_ids)
  milestone_labelling[mapping$milestone_id] <- mapping$new_milestone_id

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
get_milestone_labelling <- function(trajectory, label_milestones=NULL) {
  if(is.character(label_milestones) && length(names(label_milestones)) == length(label_milestones)) {
    labels <- label_milestones
    labels <- labels[intersect(names(labels), trajectory$milestone_ids)]
  } else if (label_milestones == TRUE) {
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

  labels
}
