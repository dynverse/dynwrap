#' @rdname add_pseudotime
#' @export
calculate_pseudotime <- function(trajectory) {
  if(!"root_milestone_id" %in% names(trajectory)) {
    warning("Trajectory is not rooted. Add a root to the trajectory using dynwrap::add_root(). This will result in an error in future releases.")
    trajectory <- add_root(trajectory)
  }

  mid <- trajectory$root_milestone_id
  mid_tempname <- paste0("MyRootMilestone", mid)

  geod <- calculate_geodesic_distances(
    trajectory,
    waypoint_cells = mid_tempname,
    waypoint_milestone_percentages = tibble(waypoint_id = mid_tempname, milestone_id = mid, percentage = 1),
    directed = TRUE
  )
  rownames(geod) <- mid

  if (nrow(geod) == 1) {
    geod[1,]
  } else {
    t(geod)
  }
}


#' Add or calculate pseudotime as distance from the root
#'
#' When calculating the pseudotime, the trajectory is expected to be rooted (see [add_root()])
#'
#' @inheritParams common_param
#' @param pseudotime Named vector containing the pseudotime for every cell. If not given, the pseudotime will be calculated.
#'
#' @return The trajectory with *pseudotime* added, which is a named vector containing the pseudotime values for every cell.
#'
#' @keywords derive_trajectory
#'
#' @seealso [add_root()], [add_linear_trajectory()]
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
  assert_that(
    is.numeric(pseudotime),
    !is.null(names(pseudotime)),
    setequal(names(pseudotime), cell_ids),
    length(names(pseudotime) )== length(cell_ids)
  )

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
