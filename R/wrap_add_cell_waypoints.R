#' Add cell waypoints to a wrapped object with trajectory
#'
#' @param object Wrapper with trajectory
#' @inheritParams select_waypoint_cells
#'
#' @importFrom testthat expect_true
#'
#' @export
add_cell_waypoints <- function(object, num_cells_selected = 100) {
  testthat::expect_true(is_wrapper_with_trajectory(object))

  waypoint_cells <- with(object, select_waypoint_cells(
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    milestone_percentages = milestone_percentages,
    progressions = progressions,
    divergence_regions = divergence_regions,
    num_cells_selected = num_cells_selected
  ))

  # create output structure
  object %>% extend_with(
    "dynwrap::with_cell_waypoints",
    waypoint_cells = waypoint_cells
  )
}

#' Test whether an object is a data_wrapper and cell waypoints
#'
#' @param object The object to be tested.
#'
#' @export
is_wrapper_with_waypoint_cells <- function(object) {
  is_wrapper_with_trajectory(object) && "dynwrap::with_cell_waypoints" %in% class(object)
}

#' Determine the positions of all cells in the trajectory
#'
#' @inheritParams add_trajectory
#'
#' @export
determine_cell_trajectory_positions <- function(
  milestone_ids,
  milestone_network,
  milestone_percentages,
  progressions,
  divergence_regions
) {
  divergence_ids <- divergence_regions$divergence_id %>% unique

  cells_in_milestone <- milestone_percentages %>%
    group_by(cell_id) %>%
    filter(percentage > 1-1e-8) %>%
    filter(n() == 1) %>%
    ungroup() %>%
    mutate(index = match(milestone_id, milestone_ids))

  cells_in_divergence <-
    map_df(seq_along(divergence_ids), function(dii) {
      mid <- divergence_regions %>%
        filter(divergence_id == divergence_ids[[dii]]) %>%
        .$milestone_id

      cells <- progressions %>%
        group_by(cell_id) %>%
        filter(n() > 1) %>%
        filter(all(unique(c(from, to)) %in% mid)) %>%
        summarise() %>%
        .$cell_id %>%
        unique

      data_frame(index = dii, cell_id = cells, divergence_id = divergence_ids[dii])
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
  )
}

#' Select the waypoint cells
#'
#' Waypoint cells are cells spread across all of the trajectory such that there is no other cell
#' that has a large geodesic distance to any of the waypoint cells.
#'
#' @inheritParams add_trajectory
#' @param num_cells_selected About the number of cells selected as waypoints
#'
#' @export
select_waypoint_cells <- function(
  milestone_ids,
  milestone_network,
  milestone_percentages,
  progressions,
  divergence_regions,
  num_cells_selected = 100
) {
  determine_cell_trajectory_positions(
    milestone_ids,
    milestone_network,
    milestone_percentages,
    progressions,
    divergence_regions
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
