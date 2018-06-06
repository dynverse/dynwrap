context("Testing add_trajectory")

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
progressions_notent <-
  progressions %>%
  group_by(cell_id) %>%
  slice(1) %>%
  ungroup()
divergence_regions <- tribble(
  ~divergence_id, ~milestone_id, ~is_start,
  "be", "in", TRUE,
  "be", "possession", FALSE,
  "be", "of", FALSE
)

wr_orig <- wrap_data(
  id = id,
  cell_ids = cell_ids,
  cell_info = cell_info,
  extras1 = extras1
)

test_that("Testing add_trajectory with milestone_percentages", {
  wr <-
    wr_orig %>%
    add_trajectory(
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

test_that("Testing add_trajectory with progressions", {
  wr <-
    wr_orig %>%
    add_trajectory(
      milestone_ids = milestone_ids,
      milestone_network = milestone_network,
      divergence_regions = divergence_regions,
      progressions = progressions,
      extras2 = extras2
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


test_that("Test empty divergence regions", {
  wr <-
    wr_orig %>%
    add_trajectory(
      milestone_ids = milestone_ids,
      milestone_network = milestone_network,
      divergence_regions = NULL,
      progressions = progressions_notent,
      extras2 = extras2
    )
  expect_true(is_data_wrapper(wr))
  expect_equivalent(nrow(wr$divergence_regions), 0)

  wr <-
    wr_orig %>%
    add_trajectory(
      milestone_ids = milestone_ids,
      milestone_network = milestone_network,
      divergence_regions = data_frame(divergence_id = character(), cell_id = character(), is_start = logical()),
      progressions = progressions_notent,
      extras2 = extras2
    )
  expect_true(is_data_wrapper(wr))
  expect_equivalent(nrow(wr$divergence_regions), 0)
})


test_that("Test combination of percentages & progressions", {
  expect_warning(
    wr_orig %>%
      add_trajectory(
        milestone_ids = milestone_ids,
        milestone_network = milestone_network,
        milestone_percentages = milestone_percentages,
        progressions = progressions,
        divergence_regions = divergence_regions,
        extras2 = extras2
      )
  )
  expect_error(
    wr_orig %>%
      add_trajectory(
        milestone_ids = milestone_ids,
        milestone_network = milestone_network,
        milestone_percentages = NULL,
        progressions = NULL,
        divergence_regions = divergence_regions,
        extras2 = extras2
      )
  )
})

test_that("Wrong milestone ids are given", {
  expect_error(
    wr_orig %>%
      add_trajectory(
        milestone_ids = 1:1000,
        milestone_network = milestone_network,
        milestone_percentages = milestone_percentages,
        divergence_regions = divergence_regions,
        extras2 = extras2
      )
  )
  expect_error(
    wr_orig %>%
      add_trajectory(
        milestone_ids = letters,
        milestone_network = milestone_network,
        milestone_percentages = milestone_percentages,
        divergence_regions = divergence_regions,
        extras2 = extras2
      )
  )
})


test_that("Wrong milestone network is given", {
  expect_error(
    wr_orig %>%
      add_trajectory(
        milestone_ids = milestone_ids,
        milestone_network = NULL,
        milestone_percentages = milestone_percentages,
        divergence_regions = divergence_regions,
        extras2 = extras2
      )
  )
  expect_error(
    wr_orig %>%
      add_trajectory(
        milestone_ids = milestone_ids,
        milestone_network = 1000L,
        milestone_percentages = milestone_percentages,
        divergence_regions = divergence_regions,
        extras2 = extras2
      )
  )
  expect_error(
    wr_orig %>%
      add_trajectory(
        milestone_ids = milestone_ids,
        milestone_network = data_frame(help = "no"),
        milestone_percentages = milestone_percentages,
        divergence_regions = divergence_regions,
        extras2 = extras2
      )
  )
  expect_error(
    wr_orig %>%
      add_trajectory(
        milestone_ids = milestone_ids,
        milestone_network = data_frame(from = "c", to = "o", length = 1, directed = FALSE),
        milestone_percentages = milestone_percentages,
        divergence_regions = divergence_regions,
        extras2 = extras2
      )
  )
  expect_error(
    wr_orig %>%
      add_trajectory(
        milestone_ids = milestone_ids,
        milestone_network = data_frame(from = 1, to = 2, length = FALSE, directed = 9),
        milestone_percentages = milestone_percentages,
        divergence_regions = divergence_regions,
        extras2 = extras2
      )
  )
})



test_that("Wrong divergence is given", {
  expect_error(
    wr_orig %>%
      add_trajectory(
        milestone_ids = milestone_ids,
        milestone_network = milestone_network,
        milestone_percentages = milestone_percentages,
        divergence_regions = NULL, # there are tented cells, so this should not work
        extras2 = extras2
      )
  )
  expect_error(
    wr_orig %>%
      add_trajectory(
        milestone_ids = milestone_ids,
        milestone_network = milestone_network,
        milestone_percentages = milestone_percentages,
        divergence_regions = 10,
        extras2 = extras2
      )
  )
  expect_error(
    wr_orig %>%
      add_trajectory(
        milestone_ids = milestone_ids,
        milestone_network = milestone_network,
        milestone_percentages = milestone_percentages,
        divergence_regions = data_frame(from = 1),
        extras2 = extras2
      )
  )
  expect_error(
    wr_orig %>%
      add_trajectory(
        milestone_ids = milestone_ids,
        milestone_network = milestone_network,
        milestone_percentages = milestone_percentages,
        divergence_regions = data_frame(divergence_id = 1, milestone_id = 1),
        extras2 = extras2
      )
  )

  expect_error(
    wr_orig %>%
      add_trajectory(
        milestone_ids = milestone_ids,
        milestone_network = milestone_network,
        milestone_percentages = milestone_percentages,
        divergence_regions = data_frame(divergence_id = 1, milestone_id = 1, is_start = 1),
        extras2 = extras2
      )
  )
  expect_error(
    wr_orig %>%
      add_trajectory(
        milestone_ids = milestone_ids,
        milestone_network = milestone_network,
        milestone_percentages = milestone_percentages,
        divergence_regions = data_frame(divergence_id = "XYZ", milestone_id = "A", is_start = TRUE),
        extras2 = extras2
      )
  )
})


test_that("Wrong milestone_percentages is given", {
  expect_error(
    wr_orig %>%
      add_trajectory(
        milestone_ids = milestone_ids,
        milestone_network = milestone_network,
        milestone_percentages = NULL,
        divergence_regions = divergence_regions,
        extras2 = extras2
      )
  )
  expect_error(
    wr_orig %>%
      add_trajectory(
        milestone_ids = milestone_ids,
        milestone_network = milestone_network,
        milestone_percentages = 1,
        divergence_regions = divergence_regions,
        extras2 = extras2
      )
  )
  expect_error(
    wr_orig %>%
      add_trajectory(
        milestone_ids = milestone_ids,
        milestone_network = milestone_network,
        milestone_percentages = data_frame(banana = "no"),
        divergence_regions = divergence_regions,
        extras2 = extras2
      )
  )
  expect_error(
    wr_orig %>%
      add_trajectory(
        milestone_ids = milestone_ids,
        milestone_network = milestone_network,
        milestone_percentages = data_frame(cell_id = 1, milestone_id = 1, percentage = FALSE),
        divergence_regions = divergence_regions,
        extras2 = extras2
      )
  )
  expect_error(
    wr_orig %>%
      add_trajectory(
        milestone_ids = milestone_ids,
        milestone_network = milestone_network,
        milestone_percentages = data_frame(cell_id = "a", milestone_id = "b", percentage = 1),
        divergence_regions = divergence_regions,
        extras2 = extras2
      )
  )
})


test_that("Wrong progressions is given", {
  expect_error(
    wr_orig %>%
      add_trajectory(
        milestone_ids = milestone_ids,
        milestone_network = milestone_network,
        progressions = NULL,
        divergence_regions = divergence_regions,
        extras2 = extras2
      )
  )
  expect_error(
    wr_orig %>%
      add_trajectory(
        milestone_ids = milestone_ids,
        milestone_network = milestone_network,
        progressions = 1,
        divergence_regions = divergence_regions,
        extras2 = extras2
      )
  )
  expect_error(
    wr_orig %>%
      add_trajectory(
        milestone_ids = milestone_ids,
        milestone_network = milestone_network,
        progressions = data_frame(banana = "no"),
        divergence_regions = divergence_regions,
        extras2 = extras2
      )
  )
  expect_error(
    wr_orig %>%
      add_trajectory(
        milestone_ids = milestone_ids,
        milestone_network = milestone_network,
        progressions = data_frame(cell_id = 1, from = 1, to = 1, percentage = FALSE),
        divergence_regions = divergence_regions,
        extras2 = extras2
      )
  )
  expect_error(
    wr_orig %>%
      add_trajectory(
        milestone_ids = milestone_ids,
        milestone_network = milestone_network,
        progressions = data_frame(cell_id = "a", from = "b", to = "c", percentage = 1),
        divergence_regions = divergence_regions,
        extras2 = extras2
      )
  )
})



test_that("Testing filtered cells", {
  wr <-
    wrap_data(
      id = id,
      cell_ids = c("1", "2", "3", "4", cell_ids)
    ) %>%
    add_trajectory(
      milestone_ids = milestone_ids,
      milestone_network = milestone_network,
      progressions = progressions,
      divergence_regions = divergence_regions,
      extras2 = extras2
    )

  filnet <- wr$milestone_network %>% filter(to == "FILTERED_CELLS")
  expect_gt(nrow(filnet), 0)
  expect_gt(max(filnet$length), max(wr$milestone_network %>% filter(to != "FILTERED_CELLS") %>% .$length))

  filmil <- wr$milestone_percentages %>%
    filter(cell_id %in% c("1", "2", "3", "4")) %>%
    .$milestone_id

  expect_true(all(filmil == "FILTERED_CELLS"))
})


test_that("Testing combination with add_grouping", {
  grouping <- sample(milestone_ids, length(cell_ids), replace = TRUE) %>% setNames(cell_ids)

  wr <-
    wrap_data(
      id = id,
      cell_ids = cell_ids
    ) %>%
    add_grouping(
      group_ids = milestone_ids,
      grouping = grouping
    ) %>%
    add_trajectory(
      milestone_ids = milestone_ids,
      milestone_network = milestone_network,
      progressions = progressions,
      divergence_regions = divergence_regions,
      extras2 = extras2
    )

  testthat::expect_equal(wr$group_ids, wr$milestone_ids)

  wr <-
    wrap_data(
      id = id,
      cell_ids = cell_ids
    ) %>%
    add_trajectory(
      milestone_ids = milestone_ids,
      milestone_network = milestone_network,
      progressions = progressions,
      divergence_regions = divergence_regions,
      extras2 = extras2
    ) %>%
    add_grouping(
      group_ids = milestone_ids,
      grouping = grouping
    )

  testthat::expect_equal(wr$group_ids, wr$milestone_ids)
})

