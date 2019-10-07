context("Testing calculate_trajectory_dimred")

id <- "a"
cell_ids <- c("truth", "universally", "acknowledged", "that", "a", "single")

milestone_ids <-  c("man", "in", "possession", "of", "good", "fortune", "must")
milestone_network <- tribble(
  ~from, ~to, ~length, ~directed,
  "man", "in", 1, TRUE,
  "in", "possession", 2, TRUE,
  "in", "of", 3, TRUE,
  "possession", "good", 4, TRUE,
  "of", "fortune", 5, TRUE,
  "good", "must", 6, TRUE,
  "fortune", "must", 7, TRUE
)
milestone_percentages <- tribble(
  ~cell_id, ~milestone_id, ~percentage,
  "truth", "man", .8,
  "truth", "in", .2,
  "universally", "in", .3,
  "universally", "possession", .2,
  "universally", "of", .5,
  "acknowledged", "possession", 0,
  "acknowledged", "good", 1,
  "that", "good", .5,
  "that", "must", .5,
  "a", "good", .9,
  "a", "must", .1,
  "single", "fortune", .6,
  "single", "must", .4
)
divergence_regions <- tribble(
  ~divergence_id, ~milestone_id, ~is_start,
  "be", "in", TRUE,
  "be", "possession", FALSE,
  "be", "of", FALSE
)

trajectory <- wrap_data(
  id = id,
  cell_ids = cell_ids
) %>% add_trajectory(
  milestone_network = milestone_network,
  divergence_regions = divergence_regions,
  milestone_percentages = milestone_percentages
)

test_that("calculate_trajectory_dimred output format is correct", {
  dimred <- calculate_trajectory_dimred(trajectory)

  expect_equal(sort(names(dimred)), c("cell_positions", "divergence_edge_positions",  "divergence_polygon_positions", "edge_positions", "milestone_positions"))

  edge_positions <- dimred$edge_positions
  expect_equal(colnames(edge_positions), c("from", "to", "length", "directed", "comp_1_from", "comp_2_from", "comp_1_to", "comp_2_to"))
  join_check <- edge_positions %>% inner_join(milestone_network, by = c("from", "to"))
  expect_equal(join_check$length.x, join_check$length.y)

  milestone_positions <- dimred$milestone_positions
  expect_equal(colnames(milestone_positions), c("milestone_id", "comp_1", "comp_2"))
  expect_true(all(milestone_ids %in% milestone_positions$milestone_id))

  cell_positions <- dimred$cell_positions
  expect_equal(colnames(cell_positions), c("cell_id", "comp_1", "comp_2"))
  expect_true(all(cell_ids %in% cell_positions$cell_id))

  divergence_edge_positions <- dimred$divergence_edge_positions
  expect_equal(colnames(divergence_edge_positions), c("from", "to", "comp_1_from", "comp_2_from", "comp_1_to", "comp_2_to"))

  divergence_polygon_positions <- dimred$divergence_polygon_positions
  expect_equal(colnames(divergence_polygon_positions), c("triangle_id", "triangle_part", "milestone_id", "comp_1", "comp_2"))
})
