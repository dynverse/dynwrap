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

  expect_equal(sort(names(dimred)), c("dimred_segments", "dimred_milestones", "dimred_cells"))

  dimred_segments <- dimred$dimred_segments
  expect_equal(colnames(dimred_segments), c("from", "to", "length", "directed", "name", "from.comp_1", "from.comp_2", "to.comp_1", "to.comp_2"))
  join_check <- dimred_segments %>% inner_join(milestone_network, by = c("from", "to"))
  expect_equal(join_check$length.x, join_check$length.y)

  dimred_milestones <- dimred$dimred_milestones
  expect_equal(colnames(dimred_milestones), c("milestone_id", "comp_1", "comp_2", "name"))
  expect_true(all(milestone_ids %in% dimred_milestones$milestone_id))

  dimred_cells <- dimred$dimred_cells
  expect_equal(colnames(dimred_cells), c("cell_id", "comp_1", "comp_2", "name"))
  expect_true(all(cell_ids %in% dimred_cells$cell_id))
})
