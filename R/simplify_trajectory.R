#' Simplify a trajectory by removing transient milestones
#'
#' Transient milestones are milestones with a degree of two (if undirected) or with an indegree of one and outdegree of one (if directed)
#'
#' The positions of the cells within the trajectory remain the same
#'
#' A → B → C is simplified to A → C.
#'
#' A — B — C is simplified to A — C.
#'
#' A → B → C → D → A is simplified to A → B → D.
#'
#' @param trajectory The trajectory as created by [infer_trajectory()] or [add_trajectory()]
#' @inheritParams simplify_igraph_network
#'
#' @keywords adapt_trajectory
#'
#' @export
simplify_trajectory <- function(trajectory, allow_self_loops = FALSE) {
  gr <- igraph::graph_from_data_frame(
    d = trajectory$milestone_network %>% rename(weight = length),
    directed = any(trajectory$milestone_network$directed),
    vertices = trajectory$milestone_ids
  )

  out <- simplify_igraph_network(
    gr,
    allow_duplicated_edges = FALSE,
    allow_self_loops = allow_self_loops,
    force_keep = unique(trajectory$divergence_regions$milestone_id),
    edge_points = trajectory$progressions %>% rename(id = cell_id)
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

  newtrajectory <- trajectory %>%
    add_trajectory(
      milestone_ids = milestone_ids,
      milestone_network = milestone_network,
      progressions = progressions,
      divergence_regions = trajectory$divergence_regions,
      allow_self_loops = allow_self_loops
    )
  # TODO: if newtrajectory contains grouping, dimred, ..., remove them as necessary
  # We need a "filter_milestones" thing for this :)

  newtrajectory
}
