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
