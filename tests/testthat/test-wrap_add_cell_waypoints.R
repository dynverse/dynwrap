context("Testing add_cell_waypoints")

orig_cell_ids <- c("a", "b", "c", "d", "e", "f")
cell_ids <- unlist(map(1:100, ~ paste0(orig_cell_ids, .)))
milestone_ids <- c("W", "X", "Y", "Z", "A")

milestone_network <- tribble(
  ~from, ~to, ~length, ~directed,
  "W", "X", 2, TRUE,
  "X", "Y", 3, TRUE,
  "X", "Z", 4, TRUE,
  "Z", "A", 5, TRUE
)

divergence_regions <- tribble(
  ~divergence_id, ~milestone_id, ~is_start,
  "XYZ", "X", TRUE,
  "XYZ", "Y", FALSE,
  "XYZ", "Z", FALSE
)

milestone_percentages <- tribble(
  ~cell_id, ~milestone_id, ~percentage,
  "a", "W", 1,
  "b", "W", .2,
  "b", "X", .8,
  "c", "X", .8,
  "c", "Z", .2,
  "d", "Z", 1,
  "e", "X", .3,
  "e", "Y", .2,
  "e", "Z", .5,
  "f", "Z", .8,
  "f", "A", .2
) %>%
  crossing(i = 1:100) %>%
  mutate(cell_id = paste0(cell_id, i)) %>%
  select(-i)

progressions <- convert_milestone_percentages_to_progressions(
  cell_ids, milestone_ids, milestone_network, milestone_percentages
)

num_samp <- 4


test_that("Testing select_waypoint_cells", {
  waypoint_cells <-
    select_waypoint_cells(
      milestone_ids,
      milestone_network,
      milestone_percentages,
      progressions,
      divergence_regions,
      num_cells_selected = length(orig_cell_ids) * num_samp
    )

  waypoint_cells_table <-
    waypoint_cells %>%
    gsub("[0-9]+", "", .) %>%
    table()

  expect_equal(names(waypoint_cells_table), orig_cell_ids)
  expect_true(all(waypoint_cells_table == num_samp))
})


test_that("Testing add_cell_waypoints", {
  trajectory <-
    wrap_data(
      id = "test",
      cell_ids = cell_ids
    ) %>% add_trajectory(
      milestone_ids = milestone_ids,
      milestone_network = milestone_network,
      milestone_percentages = milestone_percentages,
      divergence_regions = divergence_regions
    )

  trajectory2 <- add_cell_waypoints(
    trajectory,
    num_cells_selected = length(orig_cell_ids) * num_samp
  )
  waypoint_cells <-
    trajectory2$waypoint_cells

  waypoint_cells_table <-
    waypoint_cells %>%
    gsub("[0-9]+", "", .) %>%
    table()

  expect_equal(names(waypoint_cells_table), orig_cell_ids)
  expect_true(all(waypoint_cells_table == num_samp))

  expect_false(is_wrapper_with_waypoint_cells(trajectory))
  expect_true(is_wrapper_with_waypoint_cells(trajectory2))
})
