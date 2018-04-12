context("Testing compute_tented_geodesic_distances")

test_that("Testing compute_tented_geodesic_distances", {
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

  traj <- wrap_data(
    id = "test",
    cell_ids = cell_ids
  ) %>% add_trajectory_to_wrapper(
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    milestone_percentages = milestone_percentages,
    divergence_regions = divergence_regions
  )

  geodist <- compute_tented_geodesic_distances(traj)

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
  expect_true(all(abs(geodist[upper.tri(geodist)] - expected_dists) < 1e-10))

  expect_true(all(abs(geodist - t(geodist)) < 1e-10))

  # dynplot::plot_default(traj)
  # pheatmap::pheatmap(
  #   geodist,
  #   color = colorRampPalette(c("white", "#333333"))(100),
  #   cluster_cols = F, cluster_rows = F)
})
