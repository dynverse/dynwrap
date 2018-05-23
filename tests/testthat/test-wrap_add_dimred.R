context("Testing add_dimred")

# cell data
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

wr_orig <- wrap_data(
  id = id,
  cell_ids = cell_ids,
  cell_info = cell_info,
  extras1 = extras1
)

# trajectory data
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

wr_withtraj <- wr_orig %>%
  add_trajectory(
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    milestone_percentages = milestone_percentages,
    divergence_regions = divergence_regions
  )

# dimred data
num_dims <- round(runif(1, 3, 10))
dim_names <- paste0("comp_", seq_len(num_dims))

dimred <- matrix(runif(num_dims * length(cell_ids), 0, 1), nrow = length(cell_ids), dimnames = list(cell_ids, dim_names))

dimred_milestones <- matrix(runif(num_dims * length(milestone_ids), 0, 1), nrow = length(milestone_ids), dimnames = list(milestone_ids, dim_names))

dimred_trajectory_segments <- cbind(
  (dimred_milestones - .05) %>% magrittr::set_colnames(paste0("from_", dim_names)),
  (dimred_milestones + .05) %>% magrittr::set_colnames(paste0("to_", dim_names))
)

# clustering data
grouping <- sample(milestone_ids, length(cell_ids), replace = TRUE) %>% setNames(cell_ids)


test_that("Testing add_dimred", {
  wr <- wr_orig %>%
    add_dimred(
      dimred = dimred,
      extras2 = extras2
    )

  # testing is_ti_data_wrapper
  expect_true(is_wrapper_with_dimred(wr))
  expect_false(is_wrapper_with_dimred(list(chvehoie="jihofrewghifu")))

  expect_equivalent(wr$id, id)
  expect_equivalent(wr$cell_ids, cell_ids)
  expect_equivalent(wr$cell_info, cell_info)
  expect_equivalent(wr$extras1, extras1)
  expect_equivalent(wr$extras2, extras2)
  expect_equivalent(wr$dimred, dimred)
})


test_that("Testing add_dimred with traj dimred", {
  wr <- wr_withtraj %>%
    add_dimred(
      dimred = dimred,
      dimred_milestones = dimred_milestones,
      dimred_trajectory_segments = dimred_trajectory_segments,
      extras2 = extras2
    )

  # testing is_ti_data_wrapper
  expect_true(is_wrapper_with_dimred(wr))
  expect_true(is_wrapper_with_trajectory(wr))

  expect_equivalent(wr$dimred_milestones, dimred_milestones)
  expect_equivalent(wr$dimred_trajectory_segments, dimred_trajectory_segments)
})

test_that("Testing add_dimred with cell group", {
  wr <- wr_orig %>%
    add_grouping(
      group_ids = milestone_ids,
      grouping = grouping
    ) %>%
    add_dimred(
      dimred = dimred,
      dimred_milestones = dimred_milestones,
      extras2 = extras2
    )

  # testing is_ti_data_wrapper
  expect_true(is_wrapper_with_dimred(wr))
  expect_true(is_wrapper_with_grouping(wr))

  expect_equivalent(wr$dimred_milestones, dimred_milestones)
})


test_that("Expect failure on wrong dimred parameter", {
  expect_error(
    wr_withtraj %>%
      add_dimred(
        dimred = NULL,
        extras2 = extras2
      )
  )

  expect_error(
    wr_withtraj %>%
      add_dimred(
        dimred = 1,
        extras2 = extras2
      )
  )

  expect_error(
    wr_withtraj %>%
      add_dimred(
        dimred = data_frame(1, 2),
        extras2 = extras2
      )
  )

  expect_error(
    wr_withtraj %>%
      add_dimred(
        dimred = as.data.frame(dimred),
        extras2 = extras2
      )
  )
})


test_that("Expect failure on wrong dimred_milestones parameter", {
  expect_error(
    wr_withtraj %>%
      add_dimred(
        dimred = dimred,
        dimred_milestones = "vbwoc",
        extras2 = extras2
      )
  )
})


test_that("Expect failure on wrong dimred_trajectory_segments parameter", {
  expect_error(
    wr_withtraj %>%
      add_dimred(
        dimred = dimred,
        dimred_trajectory_segments = "hdcoew",
        extras2 = extras2
      )
  )
})
