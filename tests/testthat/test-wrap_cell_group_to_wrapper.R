context("Check add_cell_group_to_wrapper")

# trajectory data
id <- "a"
cell_ids <- letters
cell_group <- sample(LETTERS[1:5], length(cell_ids), replace = T) %>% setNames(cell_ids)
extras <- "banana"

wr_orig <- wrap_data(
  id = id,
  cell_ids = cell_ids
)

test_that("Testing add_cell_group_to_wrapper", {
  wr <- wr_orig %>%
    add_cell_group_to_wrapper(
      cell_group = cell_group,
      extras = extras
    )

  # testing is_ti_data_wrapper
  expect_false(is_wrapper_with_cell_group(wr_orig))
  expect_true(is_wrapper_with_cell_group(wr))
  expect_false(is_wrapper_with_cell_group(list(chvehoie="jihofrewghifu")))

  expect_equivalent(wr$id, id)
  expect_equivalent(wr$cell_ids, cell_ids)
  expect_equivalent(wr$extras, extras)
  expect_equivalent(wr$cell_group, cell_group)

  cell_group <- factor(cell_group)
  wr <- wr_orig %>%
    add_cell_group_to_wrapper(
      cell_group = cell_group,
      extras = extras
    )

  # testing is_ti_data_wrapper
  expect_false(is_wrapper_with_cell_group(wr_orig))
  expect_true(is_wrapper_with_cell_group(wr))
  expect_false(is_wrapper_with_cell_group(list(chvehoie="jihofrewghifu")))

  expect_equivalent(wr$id, id)
  expect_equivalent(wr$cell_ids, cell_ids)
  expect_equivalent(wr$extras, extras)
  expect_equivalent(wr$cell_group, cell_group)
})


test_that("Testing add_cell_group_to_wrapper with a subset of cells", {
  wr <- wr_orig %>%
    add_cell_group_to_wrapper(
      cell_group = cell_group[1:10],
      extras = extras
    )

  # testing is_ti_data_wrapper
  expect_false(is_wrapper_with_cell_group(wr_orig))
  expect_true(is_wrapper_with_cell_group(wr))
  expect_false(is_wrapper_with_cell_group(list(chvehoie="jihofrewghifu")))

  expect_equivalent(wr$id, id)
  expect_equivalent(wr$cell_ids, cell_ids)
  expect_equivalent(wr$extras, extras)
})



test_that("Testing add_cell_group_to_wrapper fails when cell_groups is not in the correct format", {
  expect_error(
    wr_orig %>%
      add_cell_group_to_wrapper(
        cell_group = cell_group %>% setNames(NULL),
        extras = extras
      )
  )

  expect_error(
    wr_orig %>%
      add_cell_group_to_wrapper(
        cell_group = rep(1, length(cell_ids)) %>% setNames(cell_ids),
        extras = extras
      )
  )

  expect_error(
    wr_orig %>%
      add_cell_group_to_wrapper(
        cell_group = data_frame(cell_group),
        extras = extras
      )
  )
  expect_error(
    wr_orig %>%
      add_cell_group_to_wrapper(
        cell_group = matrix(cell_group, ncol = 1),
        extras = extras
      )
  )
})

