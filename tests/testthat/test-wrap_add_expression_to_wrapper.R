context("Check add_expression_to_wrapper")

test_that("Testing add_expression_to_wrapper", {
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

  num_features <- round(runif(1, 100, 120))
  feature_names <- paste0("feature_", seq_len(num_features))

  expression <- matrix(runif(num_features * length(cell_ids), 8, 12), nrow = length(cell_ids), dimnames = list(cell_ids, feature_names))
  counts <- 2^expression - 1
  feature_info <- data_frame(feature_id = feature_names, mean = colMeans(expression), var = apply(expression, 2, var))

  wr <-
    wrap_data(
      id = id,
      cell_ids = cell_ids,
      cell_info = cell_info,
      extras1 = extras1
    ) %>%
    add_expression_to_wrapper(
      counts = counts,
      expression = expression,
      feature_info = feature_info,
      extras2 = extras2
    )

  # testing is_ti_data_wrapper
  expect_true(is_wrapper_with_expression(wr))
  expect_false(is_wrapper_with_expression(list(chvehoie="jihofrewghifu")))

  expect_equivalent(wr$id, id)
  expect_equivalent(wr$cell_ids, cell_ids)
  expect_equivalent(wr$cell_info, cell_info)
  expect_equivalent(wr$extras1, extras1)
  expect_equivalent(wr$extras2, extras2)
  expect_equivalent(wr$counts, counts)
  expect_equivalent(wr$expression, expression)
})
