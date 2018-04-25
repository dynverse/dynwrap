context("Testing trajectory_gene_importances")

id <- "a"
cell_ids <- c("truth", "universally", "acknowledged", "that", "a", "single")

num_features <- round(runif(1, 100, 120))
feature_names <- paste0("feature_", seq_len(num_features))

expression <- matrix(runif(num_features * length(cell_ids), 8, 12), nrow = length(cell_ids), dimnames = list(cell_ids, feature_names))
counts <- round(2^expression - 1)


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

wr <-
  wrap_data(
    id = id,
    cell_ids = cell_ids
  ) %>%
  add_expression_to_wrapper(
    counts = counts,
    expression = expression
  ) %>% add_trajectory_to_wrapper(
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    divergence_regions = divergence_regions,
    milestone_percentages = milestone_percentages
  )

test_that("Testing trajectory_gene_importances", {
  gimp <- trajectory_gene_importances(wr)

  expect_equal(gimp %>% map_chr(class), c("milestone_id" = "character", "feature_id" = "character", "importance" = "numeric"))

  expect_true(all(unique(gimp$milestone_id) %in% milestone_ids))
  expect_true(all(unique(gimp$feature_id) %in% feature_names))
})
