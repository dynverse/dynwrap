context("Check add_trajectory_to_wrapper")

test_that("Testing add_trajectory_to_wrapper with milestone_percentages", {
  id <- "a"
  cell_ids <- c("truth", "universally", "acknowledged", "that", "a", "single")
  cell_info <- data_frame(
    cell_id = cell_ids,
    info1 = c("man", "in", "possession", "of", "a", "good"),
    info2 = c("fortune", "must", "be", "in", "want", "of"),
    info3 = 1:6
  )
  extras1 <- list("a wife.")
  extras2 <- c("However", "little", "known")


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
  progressions <- tribble(
    ~cell_id, ~from, ~to, ~percentage,
    "truth", "man", "in", .2,
    "universally", "in", "possession", .2,
    "universally", "in", "of", .5,
    "acknowledged", "possession", "good", 1,
    "that", "good", "must", .5,
    "a", "good", "must", .1,
    "single", "fortune", "must", .4
  )
  divergence_regions <- tribble(
    ~divergence_id, ~milestone_id, ~is_start,
    "be", "in", TRUE,
    "be", "possession", FALSE,
    "be", "of", FALSE
  )

  wr <-
    wrap_data(
      id = id,
      cell_ids = cell_ids,
      cell_info = cell_info,
      extras1 = extras1
    ) %>%
    add_trajectory_to_wrapper(
      milestone_ids = milestone_ids,
      milestone_network = milestone_network,
      divergence_regions = divergence_regions,
      milestone_percentages = milestone_percentages,
      extras2 = extras2
    )

  # testing is_ti_data_wrapper
  expect_true(is_wrapper_with_trajectory(wr))
  expect_false(is_wrapper_with_trajectory(list(chvehoie="jihofrewghifu")))

  expect_equivalent(wr$id, id)
  expect_equivalent(wr$cell_ids, cell_ids)
  expect_equivalent(wr$cell_info, cell_info)
  expect_equivalent(wr$extras1, extras1)
  expect_equivalent(wr$extras2, extras2)
  expect_equivalent(wr$milestone_ids, milestone_ids)
  expect_equivalent(wr$milestone_network, milestone_network)
  expect_equivalent(wr$divergence_regions, divergence_regions)
  expect_equivalent(wr$milestone_percentages, milestone_percentages)
  expect_equivalent(wr$trajectory_type, "directed_acyclic_graph")

  joined <- wr$progressions %>% left_join(progressions, by = c("cell_id", "from", "to")) %>% mutate(diff = abs(percentage.x - percentage.y))
  expect_equivalent(joined$percentage.x, joined$percentage.y)
})

test_that("Testing add_trajectory_to_wrapper with milestone_percentages", {
  id <- "a"
  cell_ids <- c("truth", "universally", "acknowledged", "that", "a", "single")
  cell_info <- data_frame(
    cell_id = cell_ids,
    info1 = c("man", "in", "possession", "of", "a", "good"),
    info2 = c("fortune", "must", "be", "in", "want", "of"),
    info3 = 1:6
  )
  extras1 <- list("a wife.")
  extras2 <- c("However", "little", "known")


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
  progressions <- tribble(
    ~cell_id, ~from, ~to, ~percentage,
    "truth", "man", "in", .2,
    "universally", "in", "possession", .2,
    "universally", "in", "of", .5,
    "acknowledged", "possession", "good", 1,
    "that", "good", "must", .5,
    "a", "good", "must", .1,
    "single", "fortune", "must", .4
  )
  divergence_regions <- tribble(
    ~divergence_id, ~milestone_id, ~is_start,
    "be", "in", TRUE,
    "be", "possession", FALSE,
    "be", "of", FALSE
  )

  wr <-
    wrap_data(
      id = id,
      cell_ids = cell_ids,
      cell_info = cell_info,
      extras1 = extras1,
      extras2 = extras2
    ) %>%
    add_trajectory_to_wrapper(
      milestone_ids = milestone_ids,
      milestone_network = milestone_network,
      divergence_regions = divergence_regions,
      progressions = progressions
    )

  # testing is_ti_data_wrapper
  expect_true(is_data_wrapper(wr))
  expect_false(is_data_wrapper(list(chvehoie="jihofrewghifu")))

  expect_equivalent(wr$id, id)
  expect_equivalent(wr$cell_ids, cell_ids)
  expect_equivalent(wr$cell_info, cell_info)
  expect_equivalent(wr$extras1, extras1)
  expect_equivalent(wr$extras2, extras2)
  expect_equivalent(wr$milestone_ids, milestone_ids)
  expect_equivalent(wr$milestone_network, milestone_network)
  expect_equivalent(wr$divergence_regions, divergence_regions)
  expect_equivalent(wr$progressions, progressions)
  expect_equivalent(wr$trajectory_type, "directed_acyclic_graph")

  joined <- wr$milestone_percentages %>% left_join(milestone_percentages, by = c("cell_id", "milestone_id")) %>% mutate(diff = abs(percentage.x - percentage.y))
  expect_equivalent(joined$percentage.x, joined$percentage.y)
})
