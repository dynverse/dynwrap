#' @rdname add_pseudotime
#' @export
calculate_pseudotime <- function(trajectory) {
  if(!"root_milestone_id" %in% trajectory) {
    trajectory <- add_root(trajectory)
  }

  root_cell_id <- trajectory$milestone_percentages %>%
    filter(milestone_id == trajectory$root_milestone_id) %>%
    arrange(desc(percentage)) %>%
    pull(cell_id) %>%
    first()

  if(is.na(root_cell_id)) {stop("Could not find rooting cell for pseudotime calculation")}

  pseudotime <- compute_tented_geodesic_distances(trajectory, root_cell_id)[1, ]

  pseudotime
}


#' Calculate global pseudotime as distance from root
#'
#' @param trajectory The trajectory
#' @export
add_pseudotime <- function(trajectory) {
  trajectory$pseudotime <- calculate_pseudotime(trajectory)
  trajectory
}
