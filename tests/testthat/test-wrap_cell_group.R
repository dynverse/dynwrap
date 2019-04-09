context("Testing add_grouping")

# cell data
id <- "a"
cell_ids <- letters
group_ids <- LETTERS[1:5]
grouping <- sample(group_ids, length(cell_ids), replace = T) %>% set_names(cell_ids)
extras <- "banana"

wr_orig <- wrap_data(
  id = id,
  cell_ids = cell_ids
)

test_that("Testing add_grouping", {
  wr <- wr_orig %>%
    add_grouping(
      group_ids = group_ids,
      grouping = grouping,
      extras = extras
    )

  # testing is_ti_data_wrapper
  expect_false(is_wrapper_with_grouping(wr_orig))
  expect_true(is_wrapper_with_grouping(wr))
  expect_false(is_wrapper_with_grouping(list(chvehoie = "jihofrewghifu")))

  expect_equivalent(wr$id, id)
  expect_equivalent(wr$group_ids, group_ids)
  expect_equivalent(wr$cell_ids, cell_ids)
  expect_equivalent(wr$extras, extras)
  expect_equivalent(wr$grouping, grouping)
})


test_that("Testing add_grouping with a subset of cells", {
  wr <- wr_orig %>%
    add_grouping(
      group_ids = group_ids,
      grouping = grouping[1:10],
      extras = extras
    )

  # testing is_ti_data_wrapper
  expect_false(is_wrapper_with_grouping(wr_orig))
  expect_true(is_wrapper_with_grouping(wr))
  expect_false(is_wrapper_with_grouping(list(chvehoie = "jihofrewghifu")))

  expect_equivalent(wr$id, id)
  expect_equivalent(wr$cell_ids, cell_ids)
  expect_equivalent(wr$extras, extras)
})



test_that("Testing add_grouping fails when groupings is not in the correct format", {
  expect_error(
    wr_orig %>%
      add_grouping(
        group_ids = group_ids,
        grouping = paste0("HUO", grouping),
        extras = extras
      )
  )

  expect_error(
    wr_orig %>%
      add_grouping(
        group_ids = group_ids,
        grouping = rep(1, length(cell_ids)) %>% set_names(cell_ids),
        extras = extras
      )
  )

  expect_error(
    wr_orig %>%
      add_grouping(
        group_ids = group_ids,
        grouping = tibble(grouping),
        extras = extras
      )
  )
})

