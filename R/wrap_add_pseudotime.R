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

  pseudotime <- calculate_geodesic_distances(trajectory, root_cell_id)[1, ]

  pseudotime
}


#' Add or calculate pseudotime as distance from the root
#'
#' @param trajectory The trajectory
#' @param pseudotime Named vector containing the pseudotime for every cell
#'
#' @keywords derive_trajectory
#'
#' @export
add_pseudotime <- function(trajectory, pseudotime = NULL) {
  if (is.null(pseudotime)) {
    pseudotime <- calculate_pseudotime(trajectory)
  } else {
    pseudotime <- process_pseudotime(trajectory, pseudotime)
  }

  # check names of pseudotime
  cell_ids <- trajectory$cell_ids
  testthat::expect_is(pseudotime, "numeric")
  testthat::expect_named(pseudotime)
  testthat::expect_setequal(names(pseudotime), cell_ids)
  testthat::expect_true(length(names(pseudotime) )== length(cell_ids))

  trajectory$pseudotime <- pseudotime[trajectory$cell_ids]
  trajectory
}

process_pseudotime <- function(data_wrapper, pseudotime) {
  # convert to named vector if necessary
  if(is.data.frame(pseudotime) && all(c("cell_id", "pseudotime") %in% colnames(pseudotime))) {
    pseudotime <- pseudotime %>% select(cell_id, pseudotime) %>% deframe()
  }
  pseudotime
}
