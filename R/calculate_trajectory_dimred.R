#' Layout the trajectory and its cells in 2 dimensions using a graph layout
#'
#' @inheritParams common_param
#' @param adjust_weights Whether or not to rescale the milestone network weights
#'
#' @return A list containg
#' - *milestone_positions*: A dataframe containing the *milestone_id*
#'     and the location of each milestone (*comp_1* and *comp_2*)
#' - *edge_positions*: A dataframe containing for each edge
#'      (*from*, *to*, *length* and *directed* columns) the position
#'       of the from milestone (*comp_1_from* and *comp_2_from*) and to
#'       milestone (*comp_1_to* and *comp_2_to*).
#' - *cell_positions*: A dataframe containing the *cell_id* and the
#'   ¨location of each cell (*comp_1* and *comp_2*)
#' - *divergence_edge_positions*: A dataframe as *edge_positions*
#'    but for each edge within a divergence
#' - *divergence_polygon_positions*: A dataframe containing the *triangle_id*
#'    and the location of the milestone within a divergence (*comp_1* and *comp_2*)
#'
#' @importFrom igraph graph_from_data_frame layout_with_fr
#'
#' @examples
#' trajectory_dimred <- calculate_trajectory_dimred(example_trajectory)
#' head(trajectory_dimred$milestone_positions)
#' head(trajectory_dimred$edge_positions)
#' head(trajectory_dimred$cell_positions)
#'
#' @keywords derive_trajectory
#'
#' @export
#'
#' @seealso [wrap_data()]
calculate_trajectory_dimred <- function(
  trajectory,
  adjust_weights = FALSE
) {
  if (!is_wrapper_with_trajectory(trajectory)) {
    stop(sQuote("trajectory"), " is not a trajectory")
  }

  # expect traj to contain a trajectory
  assert_that(is_data_wrapper(trajectory))
  assert_that(is_wrapper_with_trajectory(trajectory))

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
    apply(layout[milid,,drop = FALSE], 2, function(x) sum(x * milpct)) %>% t %>% as_tibble
  }

  # create output for samples
  cell_positions <- milestone_percentages %>%
    group_by(cell_id) %>%
    do(mix_dimred(.$milestone_id, .$percentage)) %>%
    ungroup %>%
    slice(match(cell_ids, cell_id))

  # create output for milestones
  milestone_positions <- space_milest_df %>%
    rename(milestone_id = rowname)

  # create output for edges between milestones
  edge_positions <- milestone_network %>%
    left_join(space_milest_df %>% select(from = rowname, comp_1_from = comp_1, comp_2_from = comp_2), by = "from") %>%
    left_join(space_milest_df %>% select(to = rowname, comp_1_to = comp_1, comp_2_to = comp_2), by = "to") %>%
    select(from, to, length, directed, comp_1_from, comp_2_from, comp_1_to, comp_2_to)

  # extra lines and polygons for divergence regions
  if (nrow(trajectory$divergence_regions) > 0) {
    # determine the divergence triangles
    triags <- get_divergence_triangles(trajectory$divergence_regions)

    divergence_edge_positions <-
      triags %>%
      select(from = node1, to = node2) %>%
      left_join(space_milest_df %>% select(from = rowname, comp_1_from = comp_1, comp_2_from = comp_2), by = "from") %>%
      left_join(space_milest_df %>% select(to = rowname, comp_1_to = comp_1, comp_2_to = comp_2), by = "to")

    # define polygon triangles
    divergence_polygon_positions <-
      triags %>%
      mutate(triangle_id = paste0("triangle_", row_number())) %>%
      select(-divergence_id) %>%
      gather(triangle_part, milestone_id, -triangle_id) %>%
      left_join(milestone_positions, "milestone_id")
  } else {
    divergence_edge_positions <- tibble(from = character(0), to = character(0), comp_1_from = numeric(0), comp_2_from = numeric(0), comp_1_to = numeric(0), comp_2_to = numeric(0))
    divergence_polygon_positions <- tibble(triangle_id = character(0), comp_1 = numeric(0), comp_2 = numeric(0))
  }


  # return all output
  lst(
    milestone_positions,
    edge_positions,
    cell_positions,
    divergence_edge_positions,
    divergence_polygon_positions
  )
}



#' Helper function for processing divergence regions
#'
#' This function returns the combinations between
#' the start of each divergence region and pairwise combinations
#' of the end milestones.
#'
#' @param divergence_regions A divergence regions data frame as produced by `add_trajectory`.
#'
#' @noRd
get_divergence_triangles <- function(divergence_regions) {
  map_df(unique(divergence_regions$divergence_id), function(did) {
    rel_did <- divergence_regions %>% filter(divergence_id == did)

    fr <- rel_did %>% filter(is_start) %>% pull(milestone_id)
    tos <- rel_did %>% filter(!is_start) %>% pull(milestone_id)

    crossing(
      node1 = tos,
      node2 = tos
    ) %>%
      filter(node1 > node2) %>%
      mutate(
        divergence_id = did,
        start = fr
      ) %>%
      select(divergence_id, start, node1, node2)
  })
}

