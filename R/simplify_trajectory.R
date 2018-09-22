#' Simplify a trajectory
#'
#' @param traj A trajectory to simplify
#' @inheritParams simplify_igraph_network
#'
#' @export
simplify_trajectory <- function(traj, allow_self_loops = FALSE) {
  gr <- igraph::graph_from_data_frame(
    d = traj$milestone_network %>% rename(weight = length),
    directed = any(traj$milestone_network$directed),
    vertices = traj$milestone_ids
  )

  out <- simplify_igraph_network(
    gr,
    allow_duplicated_edges = FALSE,
    allow_self_loops = allow_self_loops,
    force_keep = unique(traj$divergence_regions$milestone_id),
    edge_points = traj$progressions %>% rename(id = cell_id)
  )
  milestone_ids <- igraph::V(out$gr)$name
  milestone_network <- igraph::as_data_frame(out$gr) %>%
    select(from, to, length = weight, directed) %>%
    distinct(from, to, .keep_all = TRUE)
  progressions <- out$edge_points %>%
    select(cell_id = id, from, to, percentage)

  # test whether milestone_network and progressions contain the exact same edges
  different_edges <- progressions %>% group_by(from, to) %>% summarise() %>% anti_join(milestone_network, c("from", "to"))
  if (nrow(different_edges) > 0) {
    stop("Trajectory simplification: some edges that are in the progressions are not present in the milestone network! This indicates a bug with edge flipping.")
  }

  newtraj <- traj %>%
    add_trajectory(
      milestone_ids = milestone_ids,
      milestone_network = milestone_network,
      progressions = progressions,
      divergence_regions = traj$divergence_regions,
      allow_self_loops = allow_self_loops
    )
  # TODO: if newtraj contains grouping, dimred, ..., remove them as necessary
  # We need a "filter_milestones" thing for this :)

  newtraj
}
