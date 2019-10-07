context("Testing add_end_state_probabilities")

# cell data
id <- "a"
cell_ids <- letters
end_state_ids <- LETTERS[1:5]
end_state_probabilities <- matrix(runif(length(cell_ids) * length(end_state_ids)), nrow = length(cell_ids))
colnames(end_state_probabilities) <- end_state_ids
end_state_probabilities <- end_state_probabilities %>% as.data.frame() %>% mutate(cell_id = cell_ids)
pseudotime <- runif(length(cell_ids)) %>% set_names(cell_ids)

wr_orig <- wrap_data(
  id = id,
  cell_ids = cell_ids
)

test_that("Testing add_end_state_probabilities", {
  wr <- wr_orig %>%
    add_end_state_probabilities(
      end_state_probabilities = end_state_probabilities,
      pseudotime = pseudotime
    )

  # testing is_ti_data_wrapper
  expect_false(is_wrapper_with_trajectory(wr_orig))
  expect_true(is_wrapper_with_trajectory(wr))

  expect_equivalent(wr$id, id)
  expect_equivalent(wr$pseudotime, dynutils::scale_minmax(pseudotime))
  expect_true(all(end_state_ids %in% wr$milestone_ids))
  expect_equivalent(wr$cell_ids, cell_ids)

  # test with only one end states
  wr <- wr_orig %>%
    add_end_state_probabilities(
      end_state_probabilities = end_state_probabilities[, "cell_id", drop=F],
      pseudotime = pseudotime
    )

  expect_true(nrow(wr$milestone_network) == 1)
})
