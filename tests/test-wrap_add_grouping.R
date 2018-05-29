context("Testing add_expression")

id <- "a"
cell_ids <- c("truth", "universally", "acknowledged", "that", "a", "single")
cell_info <- data_frame(
  cell_id = cell_ids,
  info1 = c("man", "in", "possession", "of", "a", "good"),
  info2 = c("fortune", "must", "be", "in", "want", "of"),
  info3 = 1:6
)

wrapper1 <- wrap_data(id, cell_ids) %>% add_grouping(cell_info$info1)
wrapper2 <- wrap_data(id, cell_ids) %>% add_grouping(unique(cell_info$info1), cell_info$info1)
wrapper3 <- wrap_data(id, cell_ids) %>% add_grouping(tibble(cell_id = cell_ids, group_id = cell_info$info1))
wrapper4 <- wrap_data(id, cell_ids, cell_info)

test_that("Testing add_grouping", {
  for (wrapper in list(wrapper1, wrapper2, wrapper3)) {
    expect_true(is_wrapper_with_grouping(wrapper))
    expect_true(all(c("grouping", "group_ids") %in% names(wrapper)))
    expect_true(all(wrapper$group_ids %in% wrapper$grouping))
    expect_equal(length(wrapper$grouping), length(wrapper$cell_ids))
    expect_equal(names(wrapper$grouping), wrapper$cell_ids)
  }
})

test_that("Testing get_grouping", {
  for (wrapper in list(wrapper1, wrapper2, wrapper3)) {
    expect_equal(get_grouping(wrapper), wrapper$grouping)
    expect_equal(names(get_grouping(wrapper)), wrapper$cell_ids)
  }

  expect_error(get_grouping(wrapper4))

  expect_equal(get_grouping(wrapper4, "info1"), wrapper3$grouping)
  expect_equal(names(get_grouping(wrapper4, "info1")), wrapper4$cell_ids)
})
