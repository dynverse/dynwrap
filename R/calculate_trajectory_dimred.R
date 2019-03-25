#' Layout the trajectory and its cells in 2 dimensions
#'
#' @inheritParams dynwrap
#' @param adjust_weights Whether or not to rescale the milestone network weights
#'
#' @importFrom igraph graph_from_data_frame layout_with_fr
#' @importFrom testthat expect_true
#'
#' @keywords derive_trajectory
#'
#' @export
#'
#' @seealso wrap_data
calculate_trajectory_dimred <- function(
  trajectory,
  adjust_weights = FALSE
) {
  if (!is_wrapper_with_trajectory(trajectory)) {
    stop(sQuote("trajectory"), " is not a trajectory")
  }

  # expect traj to contain a trajectory
  testthat::expect_true(is_data_wrapper(trajectory))
  testthat::expect_true(is_wrapper_with_trajectory(trajectory))

  # retrieve some objects to work with
  cell_ids <- trajectory$cell_ids
  milestone_ids <- trajectory$milestone_ids
  num_milestones <- length(milestone_ids)
  milestone_network <-
    trajectory$milestone_network %>%
    filter(to != "FILTERED_CELLS")
  milestone_percentages <- trajectory$milestone_percentages
  is_directed <- any(trajectory$milestone_network$directed)

  structure <- milestone_network

  # add non-visible edges between each pair of end nodes in divergences,
  # for better layout results
  if (!is.null(trajectory$divergence_regions) && nrow(trajectory$divergence_regions) > 0) {
    divergence_edges <-
      get_divergence_triangles(trajectory$divergence_regions) %>%
      left_join(milestone_network %>% select(start = from, node1 = to, length1 = length, directed), by = c("start", "node1")) %>%
      left_join(milestone_network %>% select(start = from, node2 = to, length2 = length), by = c("start", "node2")) %>%
      mutate(length = (length1 + length2) / 2) %>%
      select(from = node1, to = node2, length, directed)
    structure <- bind_rows(structure, divergence_edges)
  }

  # adjust weights on structure to make it easier to plot
  if (adjust_weights && min(structure$length) * 3 < max(structure$length)) {
    structure <- structure %>% mutate(
      length = sqrt(dynutils::scale_minmax(length) + .5)
    )
  }

  # add weights as length
  structure <- structure %>%
    mutate(weight = pmax(length, 1e-5))

  # round weights to some closest value
  # workaround for issue with igraph https://github.com/igraph/rigraph/issues/326
  structure$weight <- round(structure$weight, - log10(structure$weight) + 5)

  # reduce dimensionality on milestone_network
  gr <- igraph::graph_from_data_frame(structure, vertices = milestone_ids)
  layout <-
    igraph::layout_with_kk(gr, dim = 2, maxiter = 10000) %>%
    dynutils::scale_uniform() %>%
    set_rownames(milestone_ids) %>%
    set_colnames(paste0("comp_", seq_len(ncol(.))))
  space_milest_df <- layout %>%
    as.data.frame() %>%
    rownames_to_column()

  # project dimensionality to samples
  mix_dimred <- function(milid, milpct) {
    apply(layout[milid,,drop = FALSE], 2, function(x) sum(x * milpct)) %>% t %>% as_data_frame
  }

  # create output for samples
  dimred_cells <- milestone_percentages %>%
    group_by(cell_id) %>%
    do(mix_dimred(.$milestone_id, .$percentage)) %>%
    ungroup %>%
    slice(match(cell_ids, cell_id))

  # create output for milestones
  dimred_milestones <- space_milest_df %>%
    rename(milestone_id = rowname)

  # create output for edges between milestones
  dimred_segments <- milestone_network %>%
    left_join(space_milest_df %>% select(from = rowname, from.comp_1 = comp_1, from.comp_2 = comp_2), by = "from") %>%
    left_join(space_milest_df %>% select(to = rowname, to.comp_1 = comp_1, to.comp_2 = comp_2), by = "to")

  # extra lines and polygons for divergence regions
  if (nrow(trajectory$divergence_regions) > 0) {
    # determine the divergence triangles
    triags <- get_divergence_triangles(trajectory$divergence_regions)

    dimred_divergence_segments <-
      triags %>%
      select(from = node1, to = node2) %>%
      left_join(space_milest_df %>% select(from = rowname, from.comp_1 = comp_1, from.comp_2 = comp_2), by = "from") %>%
      left_join(space_milest_df %>% select(to = rowname, to.comp_1 = comp_1, to.comp_2 = comp_2), by = "to")

    # define polygon triangles
    dimred_divergence_polys <-
      triags %>%
      mutate(triangle_id = paste0("triangle_", row_number())) %>%
      select(-divergence_id) %>%
      gather(triangle_part, milestone_id, -triangle_id) %>%
      left_join(dimred_milestones, "milestone_id")
  } else {
    dimred_divergence_segments <- tibble(from = character(0), to = character(0), from.comp_1 = numeric(0), from.comp_2 = numeric(0), to.comp_1 = numeric(0), to.comp_2 = numeric(0))
    dimred_divergence_polys <- tibble(triangle_id = character(0), comp_1 = numeric(0), comp_2 = numeric(0))
  }


  # return all output
  lst(
    dimred_milestones,
    dimred_segments,
    dimred_cells,
    dimred_divergence_segments,
    dimred_divergence_polys
  )
}

