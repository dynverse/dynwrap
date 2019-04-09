context("Testing add_expression")

id <- "a"
cell_ids <- c("truth", "universally", "acknowledged", "that", "a", "single")
cell_info <- tibble(
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
feature_info <- tibble(feature_id = feature_names, mean = colMeans(expression), var = apply(expression, 2, var))

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

test_that("Testing add_expression and get_expression", {
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

  wrapper3 <- wrapper2
  wrapper3$expression <- function() {wrapper1$expression}

  wrappers <- list(wrapper1, wrapper2, wrapper3)

  for (wr in wrappers) {
    # testing is_wrapper_with_expression
    expect_true(is_wrapper_with_expression(wr))
    expect_false(is_wrapper_with_expression(list(chvehoie = "jihofrewghifu")))

    expect_equivalent(wr$id, id)
    expect_equivalent(wr$cell_ids, cell_ids)
    expect_equivalent(wr$cell_info, cell_info)
    expect_equivalent(wr$extras1, extras1)
    expect_equivalent(wr$extras2, extras2)

    expect_is(get_expression(wr, "counts"), "dgCMatrix")
    expect_is(get_expression(wr, "expression"), "dgCMatrix")

    expect_equivalent(get_expression(wr, "counts") %>% as.matrix, counts)
    expect_equivalent(get_expression(wr) %>% as.matrix, expression)
    expect_equivalent(get_expression("whatever", wr) %>% as.matrix, expression)
    expect_equivalent(get_expression(wr, expression) %>% as.matrix, expression)
    expect_equivalent(get_expression(wr, counts) %>% as.matrix, counts)
    expect_error(get_expression(wr, "say what"))
    expect_error(get_expression("you don't say"))
  }
})

test_that("Testing add tde_overall", {
  tde_overall <- tibble(feature_id = feature_info$feature_id) %>% mutate(differentially_expressed = runif(n()) > 0.5)
  wrapper_tde <- wrapper1 %>% add_tde_overall(tde_overall)

  expect_equal(tde_overall, wrapper_tde$tde_overall)
})





