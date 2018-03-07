select_waypoint_cells <- function(
  milestone_ids,
  milestone_network,
  milestone_percentages,
  progressions,
  divergence_regions,
  num_cells_selected = 100
) {
  divergence_ids <- divergence_regions$divergence_id %>% unique

  cells_in_milestone <- milestone_percentages %>%
    group_by(cell_id) %>%
    filter(percentage != 0) %>%
    filter(n() == 1) %>%
    ungroup() %>%
    mutate(index = match(milestone_id, milestone_ids))

  divergence_cells <-
    divergence_regions %>%
    rename(mid = milestone_id) %>%
    group_by(divergence_id) %>%
    summarise(cells = list(
      progressions %>%
        group_by(cell_id) %>%
        filter(n() > 1) %>%
        filter(all(unique(c(from, to)) %in% mid)) %>%
        ungroup() %>%
        .$cell_id %>% unique
    ))
  cells_in_divergence <-
    map_df(seq_len(nrow(divergence_cells)), function(index) {
      data_frame(index, cell_id = divergence_cells$cells[[index]])
    })

  cells_on_edge <- progressions %>%
    group_by(cell_id) %>%
    filter(n() == 1, percentage < 1-1e-8, percentage > 1e-8) %>%
    ungroup() %>%
    left_join(milestone_network %>% mutate(index = seq_len(n())) %>% select(from, to, index), by = c("from", "to"))

  places <- bind_rows(
    cells_in_milestone %>% mutate(type = "in_milestone"),
    cells_on_edge %>% mutate(type = "on_edge"),
    cells_in_divergence %>% mutate(type = "in_divergence")
  ) %>%
    group_by(type, index) %>%
    summarise(num_cells = n(), cells = list(cell_id)) %>%
    ungroup() %>%
    mutate(
      percentage = num_cells / sum(num_cells),
      num_cells_to_select = pmin(ceiling(percentage * num_cells_selected), num_cells)
    )

  waypoints <- unlist(map(seq_len(nrow(places)), function(i) {
    sample(places$cells[[i]], places$num_cells_to_select[[i]])
  }))

  unique(waypoints)
}

#' Add cell waypoints to a wrapped object with trajectory
#'
#' @param object Wrapper with trajectory
#' @param num_cells_selected About the number of cells selected as waypoints
#'
#' @importFrom testthat expect_true
#'
#' @export
add_cell_waypoints_to_wrapper <- function(object, num_cells_selected = 100) {
  testthat::expect_true(is_wrapper_with_trajectory(object))

  object$waypoint_cells <- select_waypoint_cells(
    milestone_ids = object$milestone_ids,
    milestone_network = object$milestone_network,
    milestone_percentages = object$milestone_percentages,
    progressions = object$progressions,
    divergence_regions = object$divergence_regions,
    num_cells_selected = num_cells_selected
  )

  class(object) <- c("dynutils::with_cell_waypoints", class(object))

  object
}

#' Test whether an object is a data_wrapper and cell waypoints
#'
#' @param object The object to be tested.
#'
#' @export
is_wrapper_with_waypoint_cells <- function(object) {
  is_wrapper_with_trajectory(object) && "dynutils::with_cell_waypoints" %in% class(object)
}
