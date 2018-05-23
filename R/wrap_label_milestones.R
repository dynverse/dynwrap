#' Label milestones based on highest expression of a set of markers
#'
#' @param traj The trajectory
#' @param labelling List containing for each new name of a milestone the genes which will be used as markers
#' @param expression_source The expression source
#' @param n_nearest_cells The number of nearest cells to use for extracting milestone expression
#'
#' @export
label_milestones <- function(traj, labelling, expression_source = "expression", n_nearest_cells = 20) {
  expression <- get_expression(traj, expression_source)
  milestone_ids <- traj$milestone_ids

  local_expression <- map2_df(names(labelling), labelling, function(new_milestone_id, features_oi) {
    map_df(milestone_ids, function(milestone_id) {
      cells_oi <- traj$milestone_percentages %>%
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

  traj$milestone_labelling <- milestone_labelling

  traj %>% extend_with(
    "dynwrap::with_milestone_labelling",
    milestone_labelling = milestone_labelling
  )
}



#' @rdname label_milestones
#' @export
is_wrapper_with_milestone_labelling <- function(traj) {
  is_wrapper_with_trajectory(traj) && "dynwrap::with_milestone_labelling" %in% class(traj)
}

#' @rdname label_milestones
#' @export
get_milestone_labelling <- function(traj) {
  if(is_wrapper_with_milestone_labelling(traj)) {
    traj$milestone_labelling
  } else {
    set_names(traj$milestone_ids, traj$milestone_ids)
  }
}
