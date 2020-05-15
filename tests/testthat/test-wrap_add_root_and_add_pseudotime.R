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

# dynplot::plot_graph(trajectory, label_milestones = TRUE)
test_that("Testing add_root on simple linear trajectory", {
  rooted <- add_root(trajectory, root_cell_id = "a")

  expect_true(rooted$root_milestone_id == "W")
  expect_true(rooted$milestone_network$from[[1]] == "W")
  expect_true(all(rooted$milestone_network$from == c("W", "X", "Z", "X")))
  expect_true(all(rooted$milestone_network$to == c("X", "Z", "A", "Y")))

  rooted <- add_root(trajectory)

  expect_error(add_root(trajectory, root_cell_id = "trajectories are cool"))
  expect_error(add_root(trajectory, root_milestone_id = "trajectories are cool"))
})


test_that("Testing add_root on a more complex linear trajectory", {
  trajectory2 <- wrap_data(cell_ids = "a") %>%
    add_trajectory(
      milestone_network = tibble(from = c("2", "1", "4", "3"), to = c("1", "4", "3", "5"), length = 1, directed = TRUE),
      progressions = tibble(cell_id = "a", from = "2", to = "1", percentage = 0)
    )


  trajectory2_rooted <- add_root(trajectory2, root_milestone_id = "5")

  expect_true(all(trajectory2_rooted$milestone_network$from == c("5", "3", "4", "1")))
})



test_that("Testing add_root_using_expression",{
  rooted <- add_root_using_expression(dynwrap::example_trajectory, "C")
  expect_equal(
    rooted$root_milestone_id,
    "milestone_begin"
  )
})


test_that("Testing calculate_pseudotime", {
  trajectory <- add_root(trajectory)
  trajectory <- add_pseudotime(trajectory)
  expect_equal(trajectory$pseudotime, c("a" = 0.2, "b" = 1.6, "c" = 2.8, "d" = 4.5, "e" = 4.6, "f" = 7.0))
})
