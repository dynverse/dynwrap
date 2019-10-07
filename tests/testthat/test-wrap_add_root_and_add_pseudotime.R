context("Testing add_root")

cell_ids <- c("a", "b", "c", "d", "e", "f")
milestone_ids <- c("W", "X", "Y", "Z", "A")

milestone_network <- tribble(
  ~from, ~to, ~length, ~directed,
  "W", "X", 2, TRUE,
  "X", "Z", 4, TRUE,
  "X", "Y", 3, TRUE,
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

test_that("Testing add_root", {
  rooted <- add_root(trajectory, root_cell_id = "a")

  expect_true(rooted$root_milestone_id == "W")
  expect_true(rooted$milestone_network$from[[1]] == "W")
  expect_true(all(rooted$milestone_network$from == c("W", "X", "Z", "X")))
  expect_true(all(rooted$milestone_network$to == c("X", "Z", "A", "Y")))

  rooted <- add_root(trajectory)

  expect_error(add_root(trajectory, root_cell_id = "trajectories are cool"))
  expect_error(add_root(trajectory, root_milestone_id = "trajectories are cool"))
})



test_that("Testing add_root_using_expression",{
  rooted <- add_root_using_expression(dynwrap::example_trajectory, "A")
  expect_equal(
    rooted$root_milestone_id,
    "milestone_begin"
  )
})


test_that("Testing calculate_pseudotime", {
  trajectory <- add_root(trajectory)
  trajectory <- add_pseudotime(trajectory)
  expect_equal(trajectory$pseudotime, c("a" = 0, "b" = 1.4, "c" = 2.6, "d" = 4.3, "e" = 4.4, "f" = 6.8))
})
