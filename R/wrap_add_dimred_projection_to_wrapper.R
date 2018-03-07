#' Add a dimred projection trajectory to a data wrapper
#'
#' This function will generate the milestone_network and progressions.
#'
#' @param data_wrapper A data wrapper to extend upon.
#' @param milestone_network A network of milestones.
#' @param dimred_milestones A dimensionality reduction of the milestones.
#' @param dimred_cells A dimensionality reduction of the cells.
#' @param milestone_assignment_cells A milestone assignment of the cells.
#' @param num_segments_per_edge The number of pieces each milestone transition will be split info for the calculation of the percentages.
#' @param ... extra information to be stored in the wrapper.
#'
#' @export
#'
#' @importFrom testthat expect_is expect_true expect_equal expect_length
#' @importFrom pdist pdist
add_dimred_projection_to_wrapper <- function(
  data_wrapper,
  milestone_network,
  dimred_milestones,
  dimred_cells,
  milestone_assignment_cells = NULL,
  num_segments_per_edge = 100,
  ...
) {
  # check data wrapper
  testthat::expect_is(data_wrapper, "dynutils::data_wrapper")
  cell_ids <- data_wrapper$cell_ids

  # check dimred_cells
  testthat::expect_equal(cell_ids, rownames(dimred_cells))

  # check milestone_network
  milestone_ids <- rownames(dimred_milestones)
  check_milestone_network(milestone_ids, milestone_network)

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
      sapply(colnames(dimred_cells), function(x) {
        seq(.[[paste0("from_", x)]], .[[paste0("to_", x)]], length.out = num_segments_per_edge)
      }),
      stringsAsFactors = FALSE
    )) %>%
    ungroup()

  # calculate shortest segment piece for each cell
  segment_ix <- sapply(seq_len(nrow(dimred_cells)), function(i) {
    x <- dimred_cells[i,]

    # limit possible edges based on sample cluster
    if (!is.null(milestone_assignment_cells)) {
      la <- milestone_assignment_cells[[i]]
      ix <- which(piecewise_df$from == la | piecewise_df$to == la)
    } else {
      ix <- seq_len(nrow(piecewise_df))
    }

    dis <- pdist::pdist(x, piecewise_df[ix,colnames(dimred_cells)])
    wm <- which.min(as.matrix(dis)[1,])
    ix[wm]
  })

  # construct progressions
  progressions <- data.frame(
    cell_id = rownames(dimred_cells),
    piecewise_df[segment_ix,] %>% select(from, to, percentage),
    stringsAsFactors = FALSE
  )

  # rename milestones so the milestones don't have the
  # same names as the nodes
  renamefun <- function(x) {
    y <- paste0("milestone_", x)
    if (!is.null(names(x))) {
      names(y) <- names(x)
    }
    y
  }

  progressions <- progressions %>% mutate_at(c("from", "to"), renamefun)
  milestone_network <- milestone_network %>% mutate_at(c("from", "to"), renamefun)
  milestone_ids <- renamefun(milestone_ids)
  rownames(dimred_milestones) <- renamefun(rownames(dimred_milestones))
  milestone_assignment_cells <- renamefun(milestone_assignment_cells)

  dimred_trajectory_segments <- dimred_segment_df %>%
    mutate_at(c("from", "to"), renamefun) %>%
    select(starts_with("from_"), starts_with("to_")) %>%
    magrittr::set_rownames(NULL) %>%
    as.matrix

  # return output
  add_trajectory_to_wrapper(
    data_wrapper = data_wrapper,
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    divergence_regions = NULL,
    progressions = progressions,
    milestone_assignment_cells = milestone_assignment_cells,
    ...
  ) %>%
    add_dimred_to_wrapper(
      dimred = dimred_cells,
      dimred_milestones = dimred_milestones,
      dimred_trajectory_segments = dimred_trajectory_segments
    )
}

