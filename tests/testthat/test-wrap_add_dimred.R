context("Testing add_dimred")

# cell data
id <- "a"
cell_ids <- c("truth", "universally", "acknowledged", "that", "a", "single")

counts <- matrix(
  rpois(
    length(cell_ids) * 10,
    100
  ),
  nrow = length(cell_ids), dimnames = list(cell_ids, paste0("G", seq_len(10)))
)
expression <- log2(counts + 1)

wr_orig <- wrap_data(id = id, cell_ids = cell_ids) %>%
  add_expression(counts = counts, expression = expression)

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

dimred_segment_points <- matrix(runif(num_dims * 10), ncol = num_dims)
colnames(dimred_segment_points) <- dim_names
dimred_segment_progressions <- tibble(from = "man", to = "in", percentage = seq(0, 1, length.out = nrow(dimred_segment_points)))

# clustering data
grouping <- sample(milestone_ids, length(cell_ids), replace = TRUE) %>% set_names(cell_ids)

test_that("Testing add_dimred", {
  wr <- wr_orig %>%
    add_dimred(
      dimred = dimred

    )

  # testing is_ti_data_wrapper
  expect_true(is_wrapper_with_dimred(wr))
  expect_false(is_wrapper_with_dimred(list(chvehoie = "jihofrewghifu")))

  expect_equivalent(wr$id, id)
  expect_equivalent(wr$cell_ids, cell_ids)
  expect_equivalent(wr$dimred, dimred)
})


test_that("Testing add_dimred including calculation of dimred", {
  skip_if_not_installed("dyndimre2d")
  wr <- wr_orig %>% add_dimred(dimred = dyndimred::dimred_pca)

  # testing is_ti_data_wrapper
  expect_true(is_wrapper_with_dimred(wr))
  expect_false(is_wrapper_with_dimred(list(chvehoie = "jihofrewghifu")))

  expect_equivalent(wr$id, id)
  expect_equivalent(wr$cell_ids, cell_ids)
})


test_that("Testing add_dimred with traj dimred", {
  wr <- wr_withtraj %>%
    add_dimred(
      dimred = dimred,
      dimred_milestones = dimred_milestones,
      dimred_segment_progressions = dimred_segment_progressions,
      dimred_segment_points = dimred_segment_points
    )

  # testing is_ti_data_wrapper
  expect_true(is_wrapper_with_dimred(wr))
  expect_true(is_wrapper_with_trajectory(wr))

  expect_equivalent(wr$dimred_milestones, dimred_milestones)
  expect_equivalent(wr$dimred_segment_progressions, dimred_segment_progressions)
  expect_equivalent(wr$dimred_segment_points, dimred_segment_points)
})

test_that("Testing add_dimred with cell group", {
  wr <- wr_orig %>%
    add_grouping(
      group_ids = milestone_ids,
      grouping = grouping
    ) %>%
    add_dimred(
      dimred = dimred,
      dimred_milestones = dimred_milestones
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
        dimred = NULL
      )
  )

  expect_error(
    wr_withtraj %>%
      add_dimred(
        dimred = 1
      )
  )

  expect_error(
    wr_withtraj %>%
      add_dimred(
        dimred = tibble(1, 2)
      )
  )

  expect_error(
    wr_withtraj %>%
      add_dimred(
        dimred = as.data.frame(dimred)
      )
  )
})


test_that("Expect failure on wrong dimred_milestones parameter", {
  expect_error(
    wr_withtraj %>%
      add_dimred(
        dimred = dimred,
        dimred_milestones = "vbwoc"
      )
  )
})


test_that("Expect failure on wrong dimred_segment_progressions parameter", {
  expect_error(
    wr_withtraj %>%
      add_dimred(
        dimred = dimred,
        dimred_segment_progressions = "hdcoew",
        dimred_segment_points = "hufiwe"
      )
  )

  expect_error(
    wr_withtraj %>%
      add_dimred(
        dimred = dimred,
        dimred_segment_progressions = dimred_segment_progressions,
        dimred_segment_points = "hufiwe"
      )
  )

  expect_error(
    wr_withtraj %>%
      add_dimred(
        dimred = dimred,
        dimred_segment_progressions = dimred_segment_progressions,
        dimred_segment_points = dimred_segment_points[-1,]
      )
  )

  expect_error(
    wr_withtraj %>%
      add_dimred(
        dimred = dimred,
        dimred_segment_progressions = dimred_segment_progressions,
        dimred_segment_points = dimred_segment_points[,-1]
      )
  )

  expect_error(
    wr_withtraj %>%
      add_dimred(
        dimred = dimred,
        dimred_segment_progressions = dimred_segment_progressions[-1,],
        dimred_segment_points = dimred_segment_points
      )
  )

  expect_error(
    wr_withtraj %>%
      add_dimred(
        dimred = dimred,
        dimred_segment_progressions = dimred_segment_progressions[,-1],
        dimred_segment_points = dimred_segment_points
      )
  )
})


test_that("Test add_dimred with projection", {
  wr_withdimred <- wr_withtraj %>%
    add_dimred(
      dimred = dimred,
      project_trajectory = TRUE
    )
  testthat::expect_true(!is.null(wr_withdimred$dimred_milestones))
  testthat::expect_true(!is.null(wr_withdimred$dimred_segment_points))
  testthat::expect_true(!is.null(wr_withdimred$dimred_segment_progressions))
})


test_that("Test get_dimred", {
  wr_withdimred <- wr_withtraj %>% add_dimred(dimred)

  # from trajectory
  expect_error(get_dimred(wr_orig))
  dimred2 <- get_dimred(wr_withdimred)
  expect_equivalent(dimred, dimred2)

  # function
  dimred2 <- get_dimred(wr_withdimred, dimred = dyndimred::dimred_pca)
  expect_failure(expect_equivalent(dimred, dimred2))

  # matrix
  dimred2 <- get_dimred(wr_withdimred, dimred = dimred)
  expect_equivalent(dimred, dimred2)

  # df
  dimred_df <- dimred %>% as.data.frame() %>% rownames_to_column("cell_id")
  dimred2 <- get_dimred(wr_withdimred, dimred = dimred_df)
  expect_equivalent(dimred, dimred2)

  # other
  expect_error(get_dimred(wr_orig, dimred = "yabadabadoo"))
})
