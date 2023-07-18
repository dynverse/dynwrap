#' Simplify a trajectory by removing transient milestones
#'
#' - Milestones that are not a leaf or a branching point are removed: A -> B -> C becomes A -> C
#' - Cycles contain at least 3 nodes, ie. A -> B -> A becomes A -> B -> C -> A
#' - Loops are converted to a cycle, unless `allow_self_loops = TRUE`
#'
#' The positions of the cells within the trajectory remain the same.
#' 
#' @return A trajectory object
#'
#' @inheritParams common_param
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

  # cell simplification
  edge_points <- trajectory$progressions %>% rename(id = cell_id) %>% mutate(id = paste0("SIMPLIFYCELL_", id))

  if (!is.null(trajectory$dimred_segment_points) && !is.null(trajectory$dimred_segment_progressions)) {
    edge_points <- bind_rows(
      edge_points,
      trajectory$dimred_segment_progressions %>% mutate(id = paste0("SIMPLIFYSEGMENT_", seq_len(n())))
    )
  }

  out <- simplify_igraph_network(
    gr,
    allow_duplicated_edges = FALSE,
    allow_self_loops = allow_self_loops,
    force_keep = unique(trajectory$divergence_regions$milestone_id),
    edge_points = edge_points
  )
  milestone_ids <- igraph::V(out$gr)$name
  milestone_network <- igraph::as_data_frame(out$gr) %>%
    select(from, to, length = weight, directed) %>%
    distinct(from, to, .keep_all = TRUE)
  progressions <- out$edge_points %>%
    filter(grepl("^SIMPLIFYCELL_", id)) %>%
    transmute(cell_id = gsub("^SIMPLIFYCELL_", "", id), from, to, percentage)

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

  # cell simplification
  if (!is.null(trajectory$dimred_milestones)) {
    newtrajectory$dimred_milestones <- trajectory$dimred_milestones[newtrajectory$milestone_ids, , drop = FALSE]
  }
  if (!is.null(trajectory$dimred_segment_points) && !is.null(trajectory$dimred_segment_progressions)) {
    filtered_segs <-
      out$edge_points %>%
      filter(grepl("^SIMPLIFYSEGMENT_", id)) %>%
      mutate(id = as.integer(gsub("^SIMPLIFYSEGMENT_", "", id))) %>%
      group_by(from, to) %>%
      arrange(percentage) %>%
      filter(!duplicated(percentage)) %>%
      ungroup() %>%
      arrange(from, to, percentage)
    newtrajectory$dimred_segment_points <-
      trajectory$dimred_segment_points[filtered_segs$id, , drop = FALSE]
    newtrajectory$dimred_segment_progressions <-
      filtered_segs %>% select(from, to, percentage)
  }

  # TODO: if newtrajectory contains grouping, dimred, ..., remove them as necessary
  # We need a "filter_milestones" thing for this :)

  newtrajectory
}
