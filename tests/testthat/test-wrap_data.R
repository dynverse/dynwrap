context("Testing wrap_data")

test_that("Testing data_wrapper", {
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

  wr <- wrap_data(
    id = id,
    cell_ids = cell_ids,
    cell_info = cell_info,
    extras1 = extras1,
    extras2 = extras2
  )

  # testing is_ti_data_wrapper
  expect_true(is_data_wrapper(wr))
  expect_false(is_data_wrapper(list(chvehoie = "jihofrewghifu")))

  expect_equivalent(wr$id, id)
  expect_equivalent(wr$cell_ids, cell_ids)
  expect_equivalent(wr$cell_info, cell_info)
  expect_equivalent(wr$extras1, extras1)
  expect_equivalent(wr$extras2, extras2)
})
