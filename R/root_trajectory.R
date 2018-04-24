#' Root trajectory
#'
#' Roots the trajectory by changing the directionality of all edges given a start cell
#'
#' @param trajectory the trajectory object
#' @param start_cell_id The start cell id, not required if start_milestone_id is given
#' @param start_milestone_id The start milestone id, not required if start_cell_id is given
#' @export
root_trajectory <- function(trajectory, start_cell_id = NULL, start_milestone_id = NULL) {
  if (!is.null(start_cell_id)) {
    start_milestone_id <- trajectory$milestone_percentages %>% filter(cell_id == start_cell_id) %>% filter(percentage == max(percentage)) %>% pull(milestone_id)
  } else if (is.null(start_milestone_id)) {
    message("Start cell or milestone not provided, using first milestone_id")
    start_milestone_id <- trajectory$milestone_ids[[1]]
  }

  milestone_order <- igraph::graph_from_data_frame(trajectory$milestone_network) %>% igraph::ego(nodes=start_milestone_id, 999) %>% first() %>% names()

  # first flip edge if from is later than to
  trajectory$milestone_network <- trajectory$milestone_network %>%
    mutate(
      flip = match(from, milestone_order) > match(to, milestone_order)
    )

  trajectory$progressions <- trajectory$progressions %>%
    left_join(trajectory$milestone_network %>% select(from, to, flip), c("from", "to")) %>%
    mutate(
      from2 = from,
      from = ifelse(flip, to, from),
      to = ifelse(flip, from2, to),
      percentage = ifelse(flip, 1-percentage, percentage)
    ) %>%
    select(-flip, from2)

  trajectory$milestone_network <- trajectory$milestone_network %>%
    mutate(
      from2 = from,
      from = ifelse(flip, to, from),
      to = ifelse(flip, from2, to)
    ) %>%
    select(-flip, from2)

  trajectory$root_milestone_id <- start_milestone_id

  trajectory
}



#' Calculate global pseudotime as distance from root
#'
#' @param trajectory The trajectory
#' @export
calculate_pseudotime <- function(trajectory) {
  if(!"root_milestone_id" %in% trajectory) {
    trajectory <- root_trajectory(trajectory)
  }

  start_cell_id <- trajectory$milestone_percentages %>%
    filter(milestone_id == trajectory$root_milestone_id) %>%
    arrange(desc(percentage)) %>%
    pull(cell_id) %>%
    first()

  pseudotime <- compute_tented_geodesic_distances(trajectory, start_cell_id)[1, ]

  pseudotime
}
