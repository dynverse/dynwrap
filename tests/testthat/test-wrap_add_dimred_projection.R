context("Testing add_dimred_projection")

# cell data
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

wr_orig <- wrap_data(
  id = id,
  cell_ids = cell_ids,
  cell_info = cell_info,
  extras1 = extras1
)

# trajectory data
milestone_ids <-  c("man", "in", "possession", "of", "good", "fortune", "must")
milestone_network <- tribble(
  ~from, ~to, ~length, ~directed,
  "man", "in", 1, TRUE,
  "in", "possession", 2, TRUE,
  "in", "of", 3, TRUE,
  "possession", "good", 4, TRUE,
  "of", "fortune", 5, TRUE,
  "good", "must", 6, TRUE,
  "fortune", "must", 7, TRUE
)

# grouping info
grouping <- sample(milestone_ids, length(cell_ids), replace = T) %>% set_names(cell_ids)

# dimred data
num_dims <- round(runif(1, 3, 10))
dim_names <- paste0("comp_", seq_len(num_dims))

dimred <- matrix(runif(num_dims * length(cell_ids), 0, 1), nrow = length(cell_ids), dimnames = list(cell_ids, dim_names))

dimred_milestones <- matrix(runif(num_dims * length(milestone_ids), 0, 1), nrow = length(milestone_ids), dimnames = list(milestone_ids, dim_names))



test_that("Testing add_dimred_projection", {
  wr <- wr_orig %>%
    add_dimred_projection(
      milestone_ids = milestone_ids,
      milestone_network = milestone_network,
      dimred = dimred,
      dimred_milestones = dimred_milestones,
      extras2 = extras2
    )

  # testing is_ti_data_wrapper
  expect_false(is_wrapper_with_dimred(wr_orig))
  expect_false(is_wrapper_with_trajectory(wr_orig))
  expect_true(is_wrapper_with_dimred(wr))
  expect_true(is_wrapper_with_trajectory(wr))

  expect_equivalent(wr$id, id)
  expect_equivalent(wr$cell_ids, cell_ids)
  expect_equivalent(wr$cell_info, cell_info)
  expect_equivalent(wr$extras1, extras1)
  expect_equivalent(wr$extras2, extras2)
  expect_equivalent(wr$dimred, dimred)
  expect_equivalent(wr$dimred_milestones, dimred_milestones)
  expect_equivalent(wr$milestone_ids, milestone_ids)
  expect_equivalent(wr$milestone_network, milestone_network)
})


test_that("Testing add_dimred_projection with grouping", {
  wr <- wr_orig %>%
    add_dimred_projection(
      milestone_ids = milestone_ids,
      milestone_network = milestone_network,
      dimred = dimred,
      dimred_milestones = dimred_milestones,
      grouping = grouping,
      extras2 = extras2
    )

  # testing is_ti_data_wrapper
  expect_false(is_wrapper_with_dimred(wr_orig))
  expect_false(is_wrapper_with_trajectory(wr_orig))
  expect_false(is_wrapper_with_grouping(wr_orig))
  expect_true(is_wrapper_with_dimred(wr))
  expect_true(is_wrapper_with_trajectory(wr))
  expect_true(is_wrapper_with_grouping(wr))

  expect_equivalent(wr$id, id)
  expect_equivalent(wr$cell_ids, cell_ids)
  expect_equivalent(wr$cell_info, cell_info)
  expect_equivalent(wr$extras1, extras1)
  expect_equivalent(wr$extras2, extras2)
  expect_equivalent(wr$dimred, dimred)
  expect_equivalent(wr$dimred_milestones, dimred_milestones)
  expect_equivalent(wr$milestone_ids, milestone_ids)
  expect_equivalent(wr$milestone_network, milestone_network)
  expect_equivalent(wr$grouping, grouping)

  grs <- grouping[wr$progressions$cell_id]
  expect_true(all(wr$progressions$from == grs | wr$progressions$to == grs))
})


