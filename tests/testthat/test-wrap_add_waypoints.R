context("Testing waypoints")

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

trajectory <- wrap_data("", cell_ids) %>%
  add_trajectory(milestone_ids, milestone_network, divergence_regions, milestone_percentages = milestone_percentages)


test_that("Testing select_waypoints", {
  wp <-
    select_waypoints(
      trajectory,
      n_waypoints = 100
    )

  expect_true(nrow(wp$waypoints) >= 100)
  expect_setequal(wp$waypoints$waypoint_id, wp$milestone_percentages$waypoint_id)
  expect_setequal(wp$waypoints$waypoint_id, wp$progressions$waypoint_id)
  expect_setequal(wp$waypoints$milestone_id, c(milestone_ids, NA))
  expect_setequal(wp$waypoints$waypoint_id, rownames(wp$geodesic_distances))
  expect_setequal(cell_ids, colnames(wp$geodesic_distances))
  expect_setequal(wp$waypoints$waypoint_id, c(wp$waypoint_network$from, wp$waypoint_network$to))
})


test_that("Testing add_cell_waypoints", {
  trajectory <- trajectory %>% add_waypoints()

  expect_true(!is.null(trajectory$waypoints))
})
