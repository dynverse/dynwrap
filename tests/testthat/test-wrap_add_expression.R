context("Testing add_expression")

test_that("Testing add_expression and get_expression", {
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

  wrapper1 <-
    wrap_data(
      id = id,
      cell_ids = cell_ids,
      cell_info = cell_info,
      extras1 = extras1
    ) %>%
    add_expression(
      counts = counts,
      expression = expression,
      feature_info = feature_info,
      extras2 = extras2
    )

  wrapper2 <-
    wrap_expression(
      id = id,
      expression,
      counts,
      cell_info,
      feature_info,
      extras1 = extras1,
      extras2 = extras2
    )

  wrappers <- list(wrapper1, wrapper2)

  for (wr in wrappers) {
    # testing is_wrapper_with_expression
    expect_true(is_wrapper_with_expression(wr))
    expect_false(is_wrapper_with_expression(list(chvehoie = "jihofrewghifu")))

    expect_equivalent(wr$id, id)
    expect_equivalent(wr$cell_ids, cell_ids)
    expect_equivalent(wr$cell_info, cell_info)
    expect_equivalent(wr$extras1, extras1)
    expect_equivalent(wr$extras2, extras2)
    expect_equivalent(wr$counts, counts)
    expect_equivalent(wr$expression, expression)

    expect_equivalent(get_expression(wr, "counts"), counts)
    expect_equivalent(get_expression(wr), expression)
    expect_equivalent(get_expression("whatever", wr), expression)
    expect_equivalent(get_expression(wr, expression), expression)
    expect_equivalent(get_expression(wr, counts), counts)
    expect_error(get_expression(wr, "say what"))
    expect_error(get_expression("you don't say"))
  }
})
