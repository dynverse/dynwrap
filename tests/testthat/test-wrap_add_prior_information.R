context("Testing add_prior_information")

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
expression <- matrix(rbinom(num_genes * length(cell_ids), 10000, .01), ncol = num_genes, dimnames = list(cell_ids, gene_ids))
feature_info <- tibble(
  feature_id = gene_ids,
  test = 1,
  housekeeping = sample(c(T, F), size = length(gene_ids), replace = TRUE)
)
cell_info <- tibble(
  cell_id = cell_ids,
  test = 2,
  simulationtime = runif(length(cell_ids)),
  timepoint = runif(length(cell_ids))
)


test_that("Testing generate_prior_information", {
  tmp <- tempfile()
  on.exit(file.remove(tmp))
  sink(tmp)
  prior_info <-
    generate_prior_information(
      cell_ids = cell_ids,
      milestone_ids = milestone_ids,
      milestone_network = milestone_network,
      milestone_percentages = milestone_percentages,
      progressions = progressions,
      divergence_regions = divergence_regions,
      expression = expression,
      feature_info = feature_info,
      cell_info = cell_info,
      verbose = TRUE
    )
  sink()


  expected_prior <- c(
    "start_milestones",
    "start_id",
    "end_milestones",
    "end_id",
    "groups_id",
    "groups_network",
    "features_id",
    "groups_n",
    "timecourse_continuous",
    "timecourse_discrete",
    "end_n",
    "start_n",
    "leaves_n"
  )

  testthat::expect_true(all(expected_prior %in% names(prior_info)))

  testthat::expect_equal(prior_info$start_milestones, "W")

  testthat::expect_equal(gsub("[0-9]+", "", prior_info$start_id), "a")

  testthat::expect_equal(prior_info$end_milestones %>% sort, c("A", "Y"))

  testthat::expect_equal(gsub("[0-9]+", "", prior_info$end_id), c("b", "f"))

  join_check <-
    milestone_percentages %>%
    group_by(cell_id) %>%
    arrange(desc(percentage)) %>%
    slice(1) %>%
    select(-percentage) %>%
    ungroup() %>%
    full_join(prior_info$groups_id, by = "cell_id")
  testthat::expect_equal(join_check$group_id, join_check$milestone_id)

  testthat::expect_equal(prior_info$groups_network, milestone_network %>% select(from, to))

  testthat::expect_true(all(prior_info$features_id %in% gene_ids))

  testthat::expect_equal(prior_info$groups_n, 4)

  testthat::expect_equal(prior_info$timecourse_continuous, set_names(cell_info$simulationtime, cell_info$cell_id))

  testthat::expect_equal(prior_info$timecourse_discrete, set_names(cell_info$timepoint, cell_info$cell_id))

  testthat::expect_equal(prior_info$end_n, 2)

  testthat::expect_equal(prior_info$start_n, 1)

  testthat::expect_equal(prior_info$leaves_n, 3)
})


test_that("Testing add_prior_information", {
  trajectory <-
    wrap_data(
      id = "test",
      cell_ids = cell_ids,
      cell_info = cell_info
    ) %>% add_trajectory(
      milestone_ids = milestone_ids,
      milestone_network = milestone_network,
      milestone_percentages = milestone_percentages,
      divergence_regions = divergence_regions
    ) %>% add_expression(
      counts = expression,
      expression = expression,
      feature_info = feature_info
    )

  trajectory2 <- add_prior_information(trajectory, verbose = FALSE)

  prior_info <- trajectory2$prior_information

  expect_false(is_wrapper_with_prior_information(trajectory))
  expect_true(is_wrapper_with_prior_information(trajectory2))

  # copy paste tests
  expected_prior <- c(
    "start_milestones",
    "start_id",
    "end_milestones",
    "end_id",
    "groups_id",
    "groups_network",
    "features_id",
    "groups_n",
    "timecourse_continuous",
    "timecourse_discrete",
    "start_n",
    "end_n",
    "leaves_n"
  )

  testthat::expect_true(all(expected_prior %in% names(prior_info)))

  testthat::expect_equal(prior_info$start_milestones, "W")

  testthat::expect_equal(gsub("[0-9]+", "", prior_info$start_id), "a")

  testthat::expect_equal(prior_info$end_milestones %>% sort, c("A", "Y"))

  testthat::expect_equal(gsub("[0-9]+", "", prior_info$end_id), c("b", "f"))

  join_check <-
    milestone_percentages %>%
    group_by(cell_id) %>%
    arrange(desc(percentage)) %>%
    slice(1) %>%
    select(-percentage) %>%
    ungroup() %>%
    full_join(prior_info$groups_id, by = "cell_id")
  testthat::expect_equal(join_check$group_id, join_check$milestone_id)

  testthat::expect_equal(prior_info$groups_network, milestone_network %>% select(from, to))

  testthat::expect_true(all(prior_info$features_id %in% gene_ids))

  testthat::expect_equal(prior_info$groups_n, 4)

  testthat::expect_equal(prior_info$timecourse_continuous, set_names(cell_info$simulationtime, cell_info$cell_id))

  testthat::expect_equal(prior_info$timecourse_discrete, set_names(cell_info$timepoint, cell_info$cell_id))

  testthat::expect_equal(prior_info$end_n, 2)

  testthat::expect_equal(prior_info$start_n, 1)

  testthat::expect_equal(prior_info$leaves_n, 3)
})




# with undirected cyclical dataset
orig_cell_ids <- c("a", "b", "c", "d", "e", "f")
cell_ids <- orig_cell_ids %>% map(~paste0(., seq_len(20))) %>% unlist()
orig_map <- set_names(gsub("[0-9]+", "", cell_ids), cell_ids)
milestone_ids <- c("X", "Y", "Z")

milestone_network <- tribble(
  ~from, ~to, ~length, ~directed,
  "X", "Y", 3, FALSE,
  "Y", "Z", 4, FALSE,
  "Z", "X", 5, FALSE
)

divergence_regions <- NULL

orig_progressions <- tribble(
  ~orig_cell_id, ~from, ~to, ~percentage,
  "a", "Z", "X", 1,
  "b", "X", "Y", 0.8,
  "c", "X", "Y", 1,
  "d", "Y", "Z", 0.75,
  "e", "Y", "Z", 1,
  "f", "Z", "X", .6
)
progressions <-
  tibble(cell_id = cell_ids, orig_cell_id = orig_map) %>%
  left_join(orig_progressions, by = "orig_cell_id") %>%
  mutate(
    percentage = percentage + runif(n(), -.3, .3),
    percentage = ifelse(percentage > 1, 1, ifelse(percentage < 0, 0, percentage))
  ) %>%
  select(-orig_cell_id)

milestone_percentages <-
  convert_progressions_to_milestone_percentages(
    cell_ids,
    milestone_ids,
    milestone_network,
    progressions
  )

num_genes <- 20
orig_gene_ids <- paste0("Gene", seq_len(num_genes))
orig_expression <- matrix(rbinom(num_genes * length(cell_ids), 10000, .01), ncol = num_genes, dimnames = list(cell_ids, orig_gene_ids))

milpct <- milestone_percentages %>% reshape2::acast(cell_id ~ milestone_id, value.var = "percentage", fill = 0)
expression <- cbind(orig_expression, milpct) * 100 + 2
gene_ids <- colnames(expression)

test_that("Testing generate_prior_information", {
  prior_info <-
    generate_prior_information(
      cell_ids = cell_ids,
      milestone_ids = milestone_ids,
      milestone_network = milestone_network,
      milestone_percentages = milestone_percentages,
      progressions = progressions,
      divergence_regions = divergence_regions,
      expression = expression
    )

  expected_prior <- c(
    "start_milestones",
    "start_id",
    "end_milestones",
    "end_id",
    "groups_id",
    "groups_network",
    "features_id",
    "groups_n",
    "timecourse_continuous",
    "timecourse_discrete",
    "end_n"
  )

  testthat::expect_true(all(expected_prior %in% names(prior_info)))

  join_check <-
    milestone_percentages %>%
    group_by(cell_id) %>%
    arrange(desc(percentage)) %>%
    slice(1) %>%
    select(-percentage) %>%
    ungroup() %>%
    full_join(prior_info$groups_id, by = "cell_id")
  testthat::expect_equal(join_check$group_id, join_check$milestone_id)

  testthat::expect_equal(prior_info$groups_network, milestone_network %>% select(from, to))

  testthat::expect_true(all(prior_info$features_id %in% gene_ids))

  testthat::expect_equal(prior_info$groups_n, 3)
})


test_that("Testing add_prior_information", {
  trajectory <-
    wrap_data(
      id = "test",
      cell_ids = cell_ids
    ) %>% add_trajectory(
      milestone_ids = milestone_ids,
      milestone_network = milestone_network,
      milestone_percentages = milestone_percentages,
      divergence_regions = divergence_regions
    ) %>% add_expression(
      expression = expression,
      counts = expression
    )

  tmp <- tempfile()
  on.exit(file.remove(tmp))
  sink(tmp)
  trajectory2 <- add_prior_information(trajectory, verbose = TRUE)
  sink()

  prior_info <- trajectory2$prior_information

  expect_false(is_wrapper_with_prior_information(trajectory))
  expect_true(is_wrapper_with_prior_information(trajectory2))

  # copy paste tests
  expected_prior <- c(
    "start_milestones",
    "start_id",
    "end_milestones",
    "end_id",
    "groups_id",
    "groups_network",
    "features_id",
    "groups_n",
    "timecourse_continuous",
    "timecourse_discrete",
    "end_n"
  )

  testthat::expect_true(all(expected_prior %in% names(prior_info)))

  join_check <-
    milestone_percentages %>%
    group_by(cell_id) %>%
    arrange(desc(percentage)) %>%
    slice(1) %>%
    select(-percentage) %>%
    ungroup() %>%
    full_join(prior_info$groups_id, by = "cell_id")
  testthat::expect_equal(join_check$group_id, join_check$milestone_id)

  testthat::expect_equal(prior_info$groups_network, milestone_network %>% select(from, to))

  testthat::expect_true(all(prior_info$features_id %in% gene_ids))

  testthat::expect_equal(prior_info$groups_n, 3)

})
