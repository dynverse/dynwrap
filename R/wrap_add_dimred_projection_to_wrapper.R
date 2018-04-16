#' Add a dimred projection trajectory to a data wrapper
#'
#' This function will generate the milestone_network and progressions.
#'
#' @inheritParams add_trajectory_to_wrapper
#' @inheritParams add_dimred_to_wrapper
#' @inheritParams add_cell_group_to_wrapper
#'
#' @param num_segments_per_edge The number of pieces each milestone transition will be split info for the calculation of the percentages.
#' @param ... extra information to be stored in the wrapper.
#'
#' @export
#'
#' @importFrom testthat expect_is expect_true expect_equal expect_false
#' @importFrom pdist pdist
add_dimred_projection_to_wrapper <- function(
  data_wrapper,
  milestone_ids,
  milestone_network,
  dimred,
  dimred_milestones,
  cell_group = NULL,
  num_segments_per_edge = 100,
  ...
) {
  # check data wrapper
  testthat::expect_true(is_data_wrapper(data_wrapper))

  cell_ids <- data_wrapper$cell_ids

  # check dimred
  testthat::expect_equal(cell_ids, rownames(dimred))

  # check milestone_network
  check_milestone_network(milestone_ids, milestone_network)

  # make sure milestone_ids and cell_ids don't overlap
  testthat::expect_false(any(duplicated(c(cell_ids, milestone_ids))))

  # collect information on clusters
  dimred_milestones_df <-
    dimred_milestones %>%
    data.frame(stringsAsFactors = FALSE) %>%
    rownames_to_column("milestone_id")

  # collect information on edges
  dimred_segment_df <- milestone_network %>%
    left_join(dimred_milestones_df %>% rename(from = milestone_id) %>% rename_if(is.numeric, ~ paste0("from_", .)), by = "from") %>%
    left_join(dimred_milestones_df %>% rename(to = milestone_id) %>% rename_if(is.numeric, ~ paste0("to_", .)), by = "to")

  # construct segments
  piecewise_df <- dimred_segment_df %>%
    rowwise() %>%
    do(data.frame(
      from = .$from,
      to = .$to,
      percentage = seq(0, 1, length.out = num_segments_per_edge),
      sapply(colnames(dimred), function(x) {
        seq(.[[paste0("from_", x)]], .[[paste0("to_", x)]], length.out = num_segments_per_edge)
      }),
      stringsAsFactors = FALSE
    )) %>%
    ungroup()

  # calculate shortest segment piece for each cell
  segment_ix <- sapply(seq_len(nrow(dimred)), function(i) {
    x <- dimred[i,]

    # limit possible edges based on sample cluster
    if (!is.null(cell_group)) {
      la <- cell_group[[i]]
      ix <- which(piecewise_df$from == la | piecewise_df$to == la)
    } else {
      ix <- seq_len(nrow(piecewise_df))
    }

    dis <- pdist::pdist(x, piecewise_df[ix,colnames(dimred)])
    wm <- which.min(as.matrix(dis)[1,])
    ix[wm]
  })

  # construct progressions
  progressions <- data.frame(
    cell_id = rownames(dimred),
    piecewise_df[segment_ix,] %>% select(from, to, percentage),
    stringsAsFactors = FALSE
  )

  dimred_trajectory_segments <- dimred_segment_df %>%
    select(starts_with("from_"), starts_with("to_")) %>%
    magrittr::set_rownames(NULL) %>%
    as.matrix

  # construct output
  out <- add_trajectory_to_wrapper(
    data_wrapper = data_wrapper,
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    divergence_regions = NULL,
    progressions = progressions,
    ...
  ) %>% add_dimred_to_wrapper(
    dimred = dimred,
    dimred_milestones = dimred_milestones,
    dimred_trajectory_segments = dimred_trajectory_segments
  )

  # add cell grouping of a milestone_assignment was given
  if (!is.null(cell_group)) {
    out <- out %>% add_cell_group_to_wrapper(
      group_ids = milestone_ids,
      cell_group = cell_group
    )
  }

  # return output
  out
}

