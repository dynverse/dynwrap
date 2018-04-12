#' Calculate geodesic distances between cells in a trajectory, taking into account tents
#'
#' @param trajectory The trajectory
#' @param waypoint_cells A vector of waypoint cells. Only the geodesic distances between waypoint cells and all other cells will be calculated.
#'
#' @importFrom igraph graph_from_data_frame neighborhood E distances
#' @importFrom reshape2 acast melt
#' @export
compute_tented_geodesic_distances <- function(trajectory, waypoint_cells = NULL) {
  testthat::expect_true(is_wrapper_with_trajectory(trajectory))

  # gather data from trajectory
  cell_ids <- trajectory$cell_ids

  if (is.null(waypoint_cells)) {
    waypoint_cells <- cell_ids
  }

  # rename milestones to avoid name conflicts between cells and milestones
  milestone_trafo_fun <- function(x) paste0("MILESTONE_", x)
  milestone_network <- trajectory$milestone_network %>% mutate(from = milestone_trafo_fun(from), to = milestone_trafo_fun(to))
  milestone_ids <- trajectory$milestone_ids %>% milestone_trafo_fun(.)
  milestone_percentages <- trajectory$milestone_percentages %>% mutate(milestone_id = milestone_trafo_fun(milestone_id))
  divergence_regions <- trajectory$divergence_regions %>% mutate(milestone_id = milestone_trafo_fun(milestone_id))

  # add 'extra' divergences for transitions not in a divergence
  extra_divergences <-
    milestone_network %>%
    rowwise() %>%
    mutate(in_divergence = divergence_regions %>% group_by(divergence_id) %>% summarise(match = all(c(from, to) %in% milestone_id)) %>% {any(.$match)}) %>%
    filter(!in_divergence) %>%
    do({data_frame(divergence_id = paste0(.$from, "__", .$to), milestone_id = c(.$from, .$to), is_start = c(T, F))}) %>%
    ungroup()

  divergence_regions <- bind_rows(
    divergence_regions,
    extra_divergences
  )

  # extract divergence ids
  divergence_ids <- unique(divergence_regions$divergence_id)

  # construct igraph object of milestone network
  is_directed <- any(milestone_network$directed)
  mil_gr <- igraph::graph_from_data_frame(milestone_network, directed = is_directed, vertices = milestone_ids)

  # calculate cell-cell distances for pairs of cells that are in the same tent
  cell_in_tent_distances <-
    map_df(divergence_ids, function(did) {
      dir <- divergence_regions %>% filter(divergence_id == did)
      mid <- dir %>% filter(is_start) %>% .$milestone_id
      tent <- dir$milestone_id

      tent_nomid <- setdiff(tent, mid)
      tent_distances <- igraph::distances(mil_gr, v = mid, to = tent, mode = "out", weights = igraph::E(mil_gr)$length)

      relevant_pct <- milestone_percentages %>%
        group_by(cell_id) %>%
        filter(all(milestone_id %in% tent)) %>%
        ungroup()

      if (nrow(relevant_pct) <= 1) {
        return(NULL)
      }

      scaled_dists <-
        relevant_pct %>%
        mutate(dist = percentage * tent_distances[mid, milestone_id])

      pct_mat <-
        bind_rows(
          scaled_dists %>% select(from = cell_id, to = milestone_id, length = dist),
          tent_distances %>% as.data.frame() %>% gather(from, length) %>% mutate(to = from)
        ) %>%
        reshape2::acast(from ~ to, value.var = "length", fill = 0)

      wp_cells <- rownames(pct_mat)[rownames(pct_mat) %in% waypoint_cells]

      dynutils::manhattan_distance(pct_mat, pct_mat[c(tent, wp_cells), , drop=FALSE]) %>%
        reshape2::melt(varnames = c("from", "to"), value.name = "length") %>%
        mutate_at(c("from", "to"), as.character) %>%
        filter(from != to)
    })

  # combine all networks into one graph
  gr <-
    bind_rows(milestone_network, cell_in_tent_distances) %>%
    group_by(from, to) %>%
    summarise(length = min(length)) %>%
    ungroup() %>%
    igraph::graph_from_data_frame(directed = FALSE, vertices = c(milestone_ids, cell_ids))

  # compute cell-to-cell distances across entire graph
  gr %>%
    igraph::distances(
      v = waypoint_cells,
      to = cell_ids,
      weights = igraph::E(gr)$length
    )
}
