context("Testing calculate_geodesic_distances")

test_that("Testing calculate_geodesic_distances", {
  cell_ids <- c("a", "b", "c", "d", "e", "f")
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
    "a", "W", .9,
    "a", "X", .1,
    "b", "W", .2,
    "b", "X", .8,
    "c", "X", .8,
    "c", "Z", .2,
    "d", "X", .2,
    "d", "Y", .7,
    "d", "Z", .1,
    "e", "X", .3,
    "e", "Y", .2,
    "e", "Z", .5,
    "f", "Z", .8,
    "f", "A", .2
  )

  trajectory <- wrap_data(
    id = "test",
    cell_ids = cell_ids
  ) %>% add_trajectory(
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    milestone_percentages = milestone_percentages,
    divergence_regions = divergence_regions
  )

  geodist <- calculate_geodesic_distances(trajectory)

  # a,b = 2 * (0.9 - 0.2) = 1.4
  # a,c = 2 * 0.9 + 4 * 0.2 = 2.6
  # b,c = 2 * 0.2 + 4 * 0.2 = 1.2
  # a,d = 2 * 0.9 + 3 * 0.7 + 4 * 0.1 = 4.3
  # b,d = 2 * 0.2 + 3 * 0.7 + 4 * 0.1 = 2.9
  # c,d = 3 * (0.7 - 0) + 4 * (0.2 - 0.1) = 2.5
  # a,e = 2 * 0.9 + 3 * 0.2 + 4 * 0.5 = 4.4
  # b,e = 2 * 0.2 + 3 * 0.2 + 4 * 0.5 = 3.0
  # c,e = 3 * (0.2 - 0) + 4 * (0.5 - 0.2) = 1.8
  # d,e = 3 * (0.7 - 0.2) + 4 * (0.5 - 0.1) = 3.1
  # a,f = 2 * .9 + 4 + .2 * 5 = 6.8
  # b,f = 2 * .2 + 4 + .2 * 5 = 5.4
  # c,f = .8 * 4 + .2 * 5 = 4.2
  # d,f = .7 * 3 + .9 * 4 + .2 * 5 = 6.7
  # e,f = .2 * 3 + .5 * 4 + .2 * 5 = 3.6
  expected_dists <- c(1.4, 2.6, 1.2, 4.3, 2.9, 2.5, 4.4, 3.0, 1.8, 3.1, 6.8, 5.4, 4.2, 6.7, 3.6)
  expect_equivalent(geodist[upper.tri(geodist)], expected_dists)
  expect_equivalent(geodist, t(geodist))

  # pheatmap::pheatmap(
  #   geodist,
  #   color = colorRampPalette(c("white", "#333333"))(100),
  #   cluster_cols = F, cluster_rows = F)

  # testing for waypoints
  waypoint_milestone_percentages <- tribble(
    ~milestone_id, ~waypoint_id, ~percentage,
    "W", "test1", 0.5,
    "X", "test1", 0.5,
    "X", "test2", 0.5,
    "Y", "test2", 0.1,
    "Z", "test2", 0.4
  )

  geodist <- calculate_geodesic_distances(
    trajectory,
    waypoint_milestone_percentages = waypoint_milestone_percentages
  )

  expect_true(all(c("test1", "test2") == rownames(geodist)))
  expected_dists <- c(0.8, 3.7, 0.6, 2.3, 1.8, 1.1, 3.5, 3.0, 3.6, 0.7, 6.0, 3.7)
  expect_equivalent(geodist, expected_dists)
})


test_that("Testing calculate_geodesic_distances with a gap in the middle", {
  cell_ids <- c("a", "b", "f")
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
    "a", "W", .9,
    "a", "X", .1,
    "b", "W", .2,
    "b", "X", .8,
    "f", "Z", .8,
    "f", "A", .2
  )

  trajectory <- wrap_data(
    id = "test",
    cell_ids = cell_ids
  ) %>% add_trajectory(
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    milestone_percentages = milestone_percentages,
    divergence_regions = divergence_regions
  )

  geodist <- calculate_geodesic_distances(trajectory)

  expected_dists <- c(1.4, 6.8, 5.4)
  expect_true(all(abs(geodist[upper.tri(geodist)] - expected_dists) < 1e-10))

  expect_true(all(abs(geodist - t(geodist)) < 1e-10))
})




test_that("Testing calculate_geodesic_distances with filtered cells", {
  cell_ids <- c("a", "b", "f")
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
    "a", "W", .9,
    "a", "X", .1,
    "b", "W", .2,
    "b", "X", .8
  )

  trajectory <- wrap_data(
    id = "test",
    cell_ids = cell_ids
  ) %>% add_trajectory(
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    milestone_percentages = milestone_percentages,
    divergence_regions = divergence_regions
  )

  geodist <- calculate_geodesic_distances(trajectory)

  expected_dists <- c(1.4, 14, 14)
  expect_true(all(abs(geodist[upper.tri(geodist)] - expected_dists) < 1e-10))
  expect_equal(trajectory$cell_ids, colnames(geodist))
})




test_that("Testing calculate_geodesic_distances with zero length self loops", {
  trajectory <-
    wrap_data(cell_ids = c("A", "B", "C")) %>%
    add_trajectory(
      milestone_network = tibble(from = "a", to = "a", length = 0, directed = TRUE),
      progressions = tibble(from = "a", to = "a", cell_id = c("A", "B", "C"), percentage = 1),
      allow_self_loops = TRUE
    )

  geodesic_distances <- calculate_geodesic_distances(trajectory)

  testthat::expect_true(all(geodesic_distances == 0))
})




test_that("Testing calculate_geodesic_distances while taking into account directedness", {
  # with a cycle
  milestone_network <- tribble(
    ~from, ~to, ~length, ~directed,
    "A", "B", 1, TRUE,
    "B", "C", 1, TRUE,
    "C", "A", 1, TRUE
  )
  progressions <- tribble(
    ~cell_id, ~from, ~to, ~percentage,
    "a", "A", "B", 0,
    "b", "A", "B", 0.5,
    "c", "A", "B", 1,
    "d", "B", "C", 0,
    "e", "B", "C", 0.5,
    "f", "B", "C", 1,
    "g", "C", "A", 0,
    "h", "C", "A", 0.5,
  )
  dataset <- wrap_data(cell_ids = progressions$cell_id) %>%
    add_trajectory(
      milestone_network = milestone_network,
      progressions = progressions
    )
  waypoint_cell <- "a"
  geodesic_distances <- calculate_geodesic_distances(dataset, waypoint_cells = waypoint_cell, directed = TRUE)[1, ]

  expect_equal(geodesic_distances, c(a = 0.0, b = 0.5, c = 1.0, d = 1.0, e = 1.5, f = 2.0, g = 2.0, h = 2.5))

  # with a linear
  milestone_network <- tribble(
    ~from, ~to, ~length, ~directed,
    "A", "B", 1, TRUE,
    "B", "C", 1, TRUE
  )
  progressions <- tribble(
    ~cell_id, ~from, ~to, ~percentage,
    "a", "A", "B", 0,
    "b", "A", "B", 0.5,
    "c", "A", "B", 1,
    "d", "B", "C", 0,
    "e", "B", "C", 0.5,
    "f", "B", "C", 1
  )
  dataset <- wrap_data(cell_ids = progressions$cell_id) %>%
    add_trajectory(
      milestone_network = milestone_network,
      progressions = progressions
    )
  waypoint_cell <- c("c", "f")
  geodesic_distances <- calculate_geodesic_distances(dataset, waypoint_cells = dataset$cell_ids, directed = TRUE)

  expect_equal(geodesic_distances["c", ], c(a = Inf, b = Inf, c = 0.0, d = 0.0, e = 0.5, f = 1.0))
  expect_equal(geodesic_distances["f", ], c(a = Inf, b = Inf, c = Inf, d = Inf, e = Inf, f = 0.0))
})








test_that("Testing calculate_geodesic_distances with a disconnected graph and while taking into account directedness", {
  milestone_network <- tribble(
    ~from, ~to, ~length, ~directed,
    "A", "B", 1, TRUE,
    "B", "C", 1, TRUE,
    "D", "E", 1, TRUE
  )
  progressions <- tribble(
    ~cell_id, ~from, ~to, ~percentage,
    "a", "A", "B", 0,
    "b", "A", "B", 0.5,
    "c", "A", "B", 1,
    "d", "B", "C", 0,
    "e", "B", "C", 0.5,
    "f", "B", "C", 1,
    "g", "D", "E", 0,
    "h", "D", "E", 0.5,
  )
  dataset <- wrap_data(cell_ids = progressions$cell_id) %>%
    add_trajectory(
      milestone_network = milestone_network,
      progressions = progressions
    )
  waypoint_cell <- "a"
  geodesic_distances <- calculate_geodesic_distances(dataset, waypoint_cells = waypoint_cell, directed = TRUE)[1, ]

  expect_equal(geodesic_distances, c(a = 0.0, b = 0.5, c = 1.0, d = 1.0, e = 1.5, f = 2.0, g = Inf, h = Inf))
})
