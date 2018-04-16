context("Check add_cluster_graph_to_wrapper")

# cell data
id <- "a"
cell_ids <- letters
group_ids <- LETTERS[1:5]
cell_group <- sample(group_ids, length(cell_ids), replace = T) %>% setNames(cell_ids)
extras <- "banana"

wr_orig <- wrap_data(
  id = id,
  cell_ids = cell_ids
) %>% add_cell_group_to_wrapper(
  group_ids = group_ids,
  cell_group = cell_group,
  extras = extras
)

milestone_network <- data_frame(
  from = group_ids[1:4],
  to = group_ids[2:5],
  length = c(1, 2, 4, 5),
  directed = TRUE
)

test_that("Testing add_cluster_graph_to_wrapper", {
  wr <- wr_orig %>%
    add_cluster_graph_to_wrapper(
      milestone_network = milestone_network
    )

  # testing is_ti_data_wrapper
  expect_false(is_wrapper_with_trajectory(wr_orig))
  expect_true(is_wrapper_with_trajectory(wr))

  expect_equivalent(wr$id, id)
  expect_equivalent(wr$group_ids, group_ids)
  expect_equivalent(wr$milestone_ids, group_ids)
  expect_equivalent(wr$cell_ids, cell_ids)
  expect_equivalent(wr$extras, extras)
  expect_equivalent(wr$cell_group, cell_group)
  expect_equivalent(wr$milestone_network, milestone_network)

  # percentages are either 0 or 1
  expect_true(all(abs(abs(wr$milestone_percentages$percentage - .5) - .5) < 1e-8))
})
