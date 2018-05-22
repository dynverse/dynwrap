context("Testing add_cluster_graph")

# cell data
id <- "a"
cell_ids <- letters
group_ids <- LETTERS[1:5]
grouping <- sample(group_ids, length(cell_ids), replace = T) %>% setNames(cell_ids)
extras <- "banana"

wr_orig <- wrap_data(
  id = id,
  cell_ids = cell_ids
) %>% add_grouping(
  group_ids = group_ids,
  grouping = grouping,
  extras = extras
)

milestone_network <- data_frame(
  from = group_ids[1:4],
  to = group_ids[2:5],
  length = c(1, 2, 4, 5),
  directed = TRUE
)

test_that("Testing add_cluster_graph", {
  wr <- wr_orig %>%
    add_cluster_graph(
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
  expect_equivalent(wr$grouping, grouping)
  expect_equivalent(wr$milestone_network, milestone_network)

  # percentages are either 0 or 1
  expect_true(all(abs(abs(wr$milestone_percentages$percentage - .5) - .5) < 1e-8))
})
