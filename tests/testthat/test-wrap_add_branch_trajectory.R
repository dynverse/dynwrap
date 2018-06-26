context("Testing add_trajectory")

id <- "a"
cell_ids <- letters[1:10]
branch_ids <- c("A", "B", "C", "D")

branch_network <- tribble(
  ~from, ~to,
  "A", "B",
  "B", "C",
  "B", "D"
)
branch_progressions <- tibble(
  cell_id = cell_ids,
  branch_id = c("A", "A", "A", "B", "B", "C", "C", "D", "D", "D"),
  percentage = c(0, 0.5, 1, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 1)
)
branches <- tibble(
  branch_id = branch_ids,
  length = as.numeric(1:4),
  directed = TRUE
)

wr_orig <- wrap_data(
  id = id,
  cell_ids = cell_ids
)

test_that("Testing add_trajectory with milestone_percentages", {
  wr <-
    wr_orig %>%
    add_branch_trajectory(
      branch_ids = branch_ids,
      branch_network = branch_network,
      branch_progressions = branch_progressions,
      branches = branches
    )

  # testing is_ti_data_wrapper
  expect_true(is_wrapper_with_trajectory(wr))

  expect_equivalent(wr$id, id)
  expect_equivalent(wr$cell_ids, cell_ids)
  expect_equivalent(wr$milestone_network$from, c("1", "2", "3", "3"))
  expect_equivalent(wr$milestone_network$to, c("2", "3", "4", "5"))
})
