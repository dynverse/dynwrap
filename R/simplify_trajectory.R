#' Simplify a trajectory
#'
#' @param traj A trajectory to simplify
#' @inheritParams simplify_igraph_network
#'
#' @export
simplify_trajectory <- function(traj, allow_self_loops = FALSE) {
  out <- simplify_igraph_network(
    gr = igraph::graph_from_data_frame(traj$milestone_network %>% rename(weight = length), directed = TRUE, traj$milestone_ids),
    allow_duplicated_edges = FALSE,
    allow_self_loops = allow_self_loops,
    force_keep = unique(traj$divergence_regions$milestone_id),
    edge_points = traj$progressions %>% rename(id = cell_id)
  )
  milestone_ids <- igraph::V(out$gr)$name
  milestone_network <- igraph::as_data_frame(out$gr) %>%
    select(from, to, length = weight, directed)
  progressions <- out$edge_points %>% select(cell_id = id, from, to, percentage)

  newtraj <- traj %>%
    add_trajectory(
      milestone_ids = milestone_ids,
      milestone_network = milestone_network,
      progressions = progressions,
      divergence_regions = traj$divergence_regions
    )
  # TODO: if newtraj contains grouping, dimred, ..., remove them as necessary

  newtraj
}
