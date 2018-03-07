context("Check add_dimred_to_wrapper")

test_that("Testing add_dimred_to_wrapper", {
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

  num_dims <- round(runif(1, 3, 10))
  dim_names <- paste0("comp_", seq_len(num_dims))

  dimred <- matrix(runif(num_dims * length(cell_ids), 0, 1), nrow = length(cell_ids), dimnames = list(cell_ids, dim_names))

  wr <-
    wrap_data(
      id = id,
      cell_ids = cell_ids,
      cell_info = cell_info,
      extras1 = extras1
    ) %>%
    add_dimred_to_wrapper(
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
