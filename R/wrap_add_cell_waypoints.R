#' Add or select waypoint cells of a trajectory
#'
#' Waypoint cells are cells spread across all of the trajectory such that there is no other cell
#' that has a large geodesic distance to any of the waypoint cells.
#'
#' @inheritParams add_trajectory
#' @inheritParams common_param
#' @param num_cells_selected About the number of cells selected as waypoints
#'
#' @return
#' **`add_cell_waypoints`** returns a trajectory with *waypoint_cells*, a character vector containing the cell ids of the waypoint cells
#'
#' **`select_waypoint_cells`** returns a character vector containing the cell ids of the waypoint cells
#'
#' @keywords adapt_trajectory
#' 
#' @return A dynwrap object with the waypoint cells added.
#'
#' @export
add_cell_waypoints <- function(trajectory, num_cells_selected = 100) {
  assert_that(is_wrapper_with_trajectory(trajectory))

  waypoint_cells <- with(trajectory, select_waypoint_cells(
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    milestone_percentages = milestone_percentages,
    progressions = progressions,
    divergence_regions = divergence_regions,
    num_cells_selected = num_cells_selected
  ))

  # create output structure
  trajectory %>% extend_with(
    "dynwrap::with_cell_waypoints",
    waypoint_cells = waypoint_cells
  )
}

#' @rdname add_cell_waypoints
#'
#' @export
is_wrapper_with_waypoint_cells <- function(trajectory) {
  is_wrapper_with_trajectory(trajectory) && "dynwrap::with_cell_waypoints" %in% class(trajectory)
}

#' @rdname add_cell_waypoints
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
    filter(percentage > 1-1e-6) %>%
    mutate(index = match(milestone_id, milestone_ids))

  cells_in_divergence <-
    map_df(seq_along(divergence_ids), function(dii) {
      mid <- divergence_regions %>%
        filter(divergence_id == divergence_ids[[dii]]) %>%
        filter(!is_start) %>%
        pull(milestone_id)

      mid_start <- divergence_regions %>%
        filter(divergence_id == divergence_ids[[dii]]) %>%
        filter(is_start) %>%
        pull(milestone_id)

      cells <- progressions %>%
        filter(percentage < 1-1e-6, percentage > 1e-6) %>%
        filter(from == mid_start, to %in% mid) %>%
        filter(cell_id %in% cell_id[duplicated(cell_id)]) %>%
        pull(cell_id) %>%
        unique()

      tibble(index = dii, cell_id = cells, divergence_id = divergence_ids[dii])
    })

  cells_on_edge <- progressions %>%
    filter(percentage < 1-1e-6, percentage > 1e-6) %>%
    filter(!cell_id %in% cell_id[duplicated(cell_id)]) %>%
    left_join(milestone_network %>% mutate(index = seq_len(n())) %>% select(from, to, index), by = c("from", "to"))

  places <- bind_rows(
    cells_in_milestone %>% mutate(type = "in_milestone"),
    cells_on_edge %>% mutate(type = "on_edge"),
    cells_in_divergence %>% mutate(type = "in_divergence")
  )
}

#' @rdname add_cell_waypoints
#' @export
select_waypoint_cells <- function(
  milestone_ids,
  milestone_network,
  milestone_percentages,
  progressions,
  divergence_regions,
  num_cells_selected = 100
) {
  places <- determine_cell_trajectory_positions(
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
