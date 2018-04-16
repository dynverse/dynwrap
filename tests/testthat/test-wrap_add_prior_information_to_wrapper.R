context("Testing wrap_add_prior_information_to_wrapper")

orig_cell_ids <- c("a", "b", "c", "d", "e", "f")
cell_ids <- unlist(map(1:100, ~ paste0(orig_cell_ids, .)))
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
  "a", "W", 1,
  "b", "W", .2,
  "b", "X", .8,
  "c", "X", .8,
  "c", "Z", .2,
  "d", "Z", 1,
  "e", "X", .3,
  "e", "Y", .2,
  "e", "Z", .5,
  "f", "Z", .8,
  "f", "A", .2
) %>%
  crossing(i = 1:100) %>%
  mutate(cell_id = paste0(cell_id, i)) %>%
  select(-i)

progressions <- convert_milestone_percentages_to_progressions(
  cell_ids, milestone_ids, milestone_network, milestone_percentages
)

num_genes <- 100
gene_ids <- paste0("Gene", seq_len(num_genes))
counts <- matrix(rbinom(num_genes * length(cell_ids), 1000, .01), ncol = num_genes, dimnames = list(cell_ids, gene_ids))
feature_info <- data_frame(feature_id = gene_ids, test = 1)
cell_info <- data_frame(
  cell_id = cell_ids,
  test = 2,
  simulationtime = runif(length(cell_ids)),
  timepoint = runif(length(cell_ids))
)


test_that("Testing generate_prior_information", {
  prior_info <-
    generate_prior_information(
      milestone_ids = milestone_ids,
      milestone_network = milestone_network,
      milestone_percentages = milestone_percentages,
      progressions = progressions,
      divergence_regions = divergence_regions,
      counts = counts,
      feature_info = feature_info,
      cell_info = cell_info
    )

  expected_prior <- c(
    "start_milestones",
    "start_cells",
    "end_milestones",
    "end_cells",
    "grouping_assignment",
    "grouping_network",
    "marker_feature_ids",
    "n_branches",
    "time",
    "timecourse",
    "n_end_states"
  )

  testthat::expect_true(all(expected_prior %in% names(prior_info)))

  testthat::expect_equal(prior_info$start_milestones, "W")

  testthat::expect_equal(str_replace(prior_info$start_cells, "[0-9]+", ""), "a")

  testthat::expect_equal(prior_info$end_milestones %>% sort, c("A", "Y"))

  testthat::expect_equal(str_replace(prior_info$end_cells, "[0-9]+", ""), c("b", "f"))

  join_check <-
    milestone_percentages %>%
    group_by(cell_id) %>%
    arrange(desc(percentage)) %>%
    slice(1) %>%
    select(-percentage) %>%
    ungroup() %>%
    full_join(prior_info$grouping_assignment, by = "cell_id")
  testthat::expect_equal(join_check$group_id, join_check$milestone_id)

  testthat::expect_equal(prior_info$grouping_network, milestone_network %>% select(from, to))

  testthat::expect_true(all(prior_info$marker_feature_ids %in% gene_ids))

  testthat::expect_equal(prior_info$n_branches, 4)

  testthat::expect_equal(prior_info$time, set_names(cell_info$simulationtime, cell_info$cell_id))

  testthat::expect_equal(prior_info$timecourse, set_names(cell_info$timepoint, cell_info$cell_id))

  testthat::expect_equal(prior_info$n_end_states, 2)

})


test_that("Testing add_prior_information_to_wrapper", {
  traj <-
    wrap_data(
      id = "test",
      cell_ids = cell_ids
    ) %>% add_trajectory_to_wrapper(
      milestone_ids = milestone_ids,
      milestone_network = milestone_network,
      milestone_percentages = milestone_percentages,
      divergence_regions = divergence_regions
    ) %>% add_expression_to_wrapper(
      counts = counts,
      expression = log2(counts+1),
      feature_info = feature_info
    )

  traj2 <- add_prior_information_to_wrapper(traj)

  prior_info2 <- traj2$prior_information

  expect_equal(prior_info2, prior_info)

  expect_false(is_wrapper_with_waypoint_cells(traj))
  expect_true(is_wrapper_with_waypoint_cells(traj2))
})

