#' Constructs a trajectory by projecting cells within a dimensionality reduction onto a backbone formed by a milestone network. Optionally, a cell grouping can be given which will restrict the edges on which a cell can be projected.
#'
#' This function will generate the milestone_network and progressions.
#'
#' @inheritParams add_trajectory
#' @inheritParams add_dimred
#' @inheritParams add_grouping
#'
#' @param ... extra information to be stored in the wrapper.
#'
#' @export
#'
#' @return The trajectory model
#'
#' @importFrom testthat expect_is expect_true expect_equal expect_false
#' @importFrom pdist pdist
add_dimred_projection <- function(
  model,
  milestone_ids = NULL,
  milestone_network,
  dimred,
  dimred_milestones,
  grouping = NULL,
  ...
) {
  # check data wrapper
  testthat::expect_true(is_data_wrapper(model))

  cell_ids <- model$cell_ids

  # process milestone_ids
  if(is.null(milestone_ids)) {
    milestone_ids <- unique(c(milestone_network$from, milestone_network$to))
  }

  # process grouping
  if(!is.null(grouping)) {
    grouping <- process_grouping(model, grouping)
  }

  # add dimred and dimred_milestones
  dimred <- process_dimred(model, dimred)
  dimred_milestones <- process_dimred(model, dimred_milestones, "milestone_id")
  testthat::expect_setequal(milestone_ids, rownames(dimred_milestones))

  # check milestone_network
  check_milestone_network(milestone_ids, milestone_network)

  # make sure milestone_ids and cell_ids don't overlap
  testthat::expect_false(any(duplicated(c(cell_ids, milestone_ids))))

  if (is.null(grouping)) {
    # if no grouping is given, just project all cells to the segments
    proj <- dynutils::project_to_segments(
      x = dimred,
      segment_start = dimred_milestones[milestone_network$from, , drop = FALSE],
      segment_end = dimred_milestones[milestone_network$to, , drop = FALSE]
    )
    progressions <-
      milestone_network %>%
      slice(proj$segment) %>%
      mutate(
        cell_id = names(proj$segment),
        percentage = proj$progression
      ) %>%
      select(cell_id, from, to, percentage)
  } else {
    # if grouping / clusterings are given, project cells only to segments
    # of which either the from or the to is equal to their grouping
    group_ids <- unique(grouping)

    progressions <- map_df(group_ids, function(group_id) {
      cids <- names(which(grouping == group_id))

      # select all cells in this group
      if (length(cids) > 0) {
        mns <- milestone_network %>% filter(from == group_id | to == group_id)

        if (nrow(mns) > 0) {
          # project to segments
          proj <- dynutils::project_to_segments(
            x = dimred[cids, , drop = FALSE],
            segment_start = dimred_milestones[mns$from, , drop = FALSE],
            segment_end = dimred_milestones[mns$to, , drop = FALSE]
          )
          data_frame(
            cell_id = cids,
            from = mns$from[proj$segment],
            to = mns$to[proj$segment],
            percentage = proj$progression
          )
        } else {
          # this group is a separate cluster
          data_frame(
            cell_id = cids,
            from = group_id,
            to = group_id,
            percentage = 1
          )
        }
      } else {
        NULL
      }
    })

    # add missing group ids as clusters to the milestone network
    missing_gids <- group_ids %>% setdiff(c(milestone_network$from, milestone_network$to))
    milestone_network <- milestone_network %>%
      bind_rows(data_frame(from = missing_gids, to = missing_gids, length = 0, directed = FALSE))
  }

  # collect information on clusters
  dimred_milestones_df <-
    dimred_milestones %>%
    data.frame(stringsAsFactors = FALSE) %>%
    rownames_to_column("milestone_id")
  dimred_trajectory_segments <- milestone_network %>%
    left_join(dimred_milestones_df %>% rename(from = milestone_id) %>% rename_if(is.numeric, ~ paste0("from_", .)), by = "from") %>%
    left_join(dimred_milestones_df %>% rename(to = milestone_id) %>% rename_if(is.numeric, ~ paste0("to_", .)), by = "to") %>%
    select(starts_with("from_"), starts_with("to_")) %>%
    magrittr::set_rownames(NULL) %>%
    as.matrix

  # construct output
  out <- add_trajectory(
    model = model,
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    divergence_regions = NULL,
    progressions = progressions,
    ...
  ) %>% add_dimred(
    dimred = dimred,
    dimred_milestones = dimred_milestones,
    dimred_trajectory_segments = dimred_trajectory_segments
  )

  # add cell grouping of a milestone_assignment was given
  if (!is.null(grouping)) {
    out <- out %>% add_grouping(
      group_ids = milestone_ids,
      grouping = grouping
    )
  }

  # return output
  out
}
