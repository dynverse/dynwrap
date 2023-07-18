#' Constructs a trajectory by projecting cells within a dimensionality reduction
#'
#' A dimensionality reduction of cells and milestones is used, along with the milestone network, to project cells onto the nearest edge. Optionally, a cell grouping can be given which will restrict the edges on which a cell can be projected.
#'
#' @inheritParams common_param
#' @inheritParams add_trajectory
#' @inheritParams add_dimred
#' @inheritParams add_grouping
#' @param ... extra information to be stored in the wrapper.
#'
#' @inherit add_trajectory return
#'
#' @keywords create_trajectory
#' 
#' @return A trajectory object
#'
#' @export
#'
#' @examples
#' library(tibble)
#' dataset <- wrap_data(cell_ids = letters)
#'
#' milestone_network <- tibble::tibble(
#'   from = c("A", "B", "B"),
#'   to = c("B", "C", "D"),
#'   directed = TRUE,
#'   length = 1
#' )
#' milestone_network
#' dimred <- matrix(
#'   runif(length(dataset$cell_ids) * 2),
#'   ncol = 2,
#'   dimnames = list(dataset$cell_ids, c("comp_1", "comp_2"))
#' )
#' dimred
#' dimred_milestones <- matrix(
#'   runif(2*4),
#'   ncol = 2,
#'   dimnames = list(c("A", "B", "C", "D"), c("comp_1", "comp_2"))
#' )
#' dimred_milestones
#' trajectory <- add_dimred_projection(
#'   dataset,
#'   milestone_network = milestone_network,
#'   dimred = dimred,
#'   dimred_milestones = dimred_milestones
#' )
#'
#' # for plotting the result, install dynplot
#' #- dynplot::plot_graph(trajectory)
add_dimred_projection <- function(
  dataset,
  milestone_ids = NULL,
  milestone_network,
  dimred,
  dimred_milestones,
  grouping = NULL,
  ...
) {
  # check data wrapper
  assert_that(is_data_wrapper(dataset))

  cell_ids <- dataset$cell_ids

  # process milestone_ids
  if(is.null(milestone_ids)) {
    milestone_ids <- unique(c(milestone_network$from, milestone_network$to))
  }

  # process grouping
  if(!is.null(grouping)) {
    grouping <- process_grouping(dataset, grouping)
  }

  # add dimred and dimred_milestones
  dimred <- process_dimred(dataset, dimred)
  dimred_milestones <- process_dimred(dataset, dimred_milestones, "milestone_id")
  assert_that(setequal(milestone_ids, rownames(dimred_milestones)))

  # check milestone_network
  check_milestone_network(milestone_ids, milestone_network)

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
          tibble(
            cell_id = cids,
            from = mns$from[proj$segment],
            to = mns$to[proj$segment],
            percentage = proj$progression
          )
        } else {
          # this group is a separate cluster
          tibble(
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
      bind_rows(tibble(from = missing_gids, to = missing_gids, length = 0, directed = FALSE))
  }

  dimred_segment_progressions <-
    milestone_network %>%
    select(from, to) %>%
    mutate(zero = from, one = to) %>%
    gather(percentage, milestone_id, zero, one) %>%
    mutate(percentage = c(zero = 0, one = 1)[percentage])

  dimred_segment_points <-
    dimred_milestones[dimred_segment_progressions$milestone_id, , drop = FALSE]

  dimred_segment_progressions <-
    dimred_segment_progressions %>%
    select(from, to, percentage)

  # construct output
  out <- add_trajectory(
    dataset = dataset,
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    divergence_regions = NULL,
    progressions = progressions,
    ...
  ) %>% add_dimred(
    dimred = dimred,
    dimred_milestones = dimred_milestones,
    dimred_segment_points = dimred_segment_points,
    dimred_segment_progressions = dimred_segment_progressions
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
