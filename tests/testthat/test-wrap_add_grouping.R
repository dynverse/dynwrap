context("Testing add_grouping")

id <- "a"
cell_ids <- c("truth", "universally", "acknowledged", "that", "a", "single")
cell_info <- tibble(
  cell_id = cell_ids,
  info1 = c("man", "in", "possession", "of", "a", "good"),
  info2 = c("fortune", "must", "be", "in", "want", "of"),
  info3 = 1:6
)

wrapper1 <- wrap_data(id, cell_ids) %>% add_grouping(cell_info$info1)
wrapper2 <- wrap_data(id, cell_ids) %>% add_grouping(unique(cell_info$info1), cell_info$info1)
wrapper3 <- wrap_data(id, cell_ids) %>% add_grouping(tibble(cell_id = cell_ids, group_id = cell_info$info1))
wrapper4 <- wrap_data(id, cell_ids) %>% add_prior_information(groups_id = tibble(cell_id = cell_ids, group_id = cell_info$info1))
wrapper5 <- wrap_data(id, cell_ids, cell_info)

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
  for (wrapper in list(wrapper1, wrapper2, wrapper3, wrapper4)) {
    expect_equal(get_grouping(wrapper), wrapper1$grouping)
    expect_equal(names(get_grouping(wrapper)), wrapper$cell_ids)
  }

  expect_error(get_grouping(wrapper5))

  expect_equal(get_grouping(wrapper5, "info1"), wrapper3$grouping)
  expect_equal(names(get_grouping(wrapper5, "info1")), wrapper5$cell_ids)
})


milestone_network <- tibble(from = c("A", "B"), to = c("B", "C"), directed = TRUE, length = 1)
progressions <- tibble(cell_id = cell_ids, from = c(rep("A", 3), rep("B", 3)), to = c(rep("B", 3), rep("C", 3)), percentage = c(0, 0.5, 1, 0, 0.5, 1))

trajectory <- wrap_data(id, cell_ids) %>% add_trajectory(milestone_network = milestone_network, progressions = progressions)

test_that("Testing group_onto_trajectory_edges", {
  grouping <- group_onto_trajectory_edges(trajectory)

  expect_equal(length(grouping), length(cell_ids))
  expect_equal(names(grouping), cell_ids)
  expect_equal(grouping %>% unname(), c("A->B", "A->B", "A->B", "B->C", "B->C", "B->C"))
})
