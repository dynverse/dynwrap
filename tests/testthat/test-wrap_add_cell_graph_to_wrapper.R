context("Testing add_cell_graph_to_wrapper")


# cell data
cell_ids <- c("A", "B", "C", "D", "E", "F", "a", "b", "bb", "c", "cc", "d")

wr_orig <- wrap_data(
  id = "test",
  cell_ids = cell_ids
)


test_that("Testing add_cell_graph_to_wrapper", {
  cell_graph <- tibble::tribble(
    ~from, ~to, ~length, ~directed,
    "A", "B", .5, F,
    "B", "C", .6, F,
    "C", "D", .7, F,
    "D", "E", .8, F,
    "D", "F", .9, F,
    "a", "A", .1, F,
    "b", "B", .1, F,
    "bb", "B", .08, F,
    "c", "C", .05, F,
    "cc", "c", .1, F,
    "d", "D", .01, F
  )
  to_keep <- c(A = T, B = T, C = T, D = T, E = T, "F" = T, a = F, b = F, bb = F, c = F, cc = F, d = F)

  wr <- wr_orig %>% add_cell_graph_to_wrapper(
    cell_graph = cell_graph,
    to_keep = to_keep,
    milestone_prefix = "ML_"
  )

  # testing is_ti_data_wrapper
  expect_false(is_wrapper_with_trajectory(wr_orig))
  expect_true(is_wrapper_with_trajectory(wr))

  # testing milestone ids
  expect_equal(wr$cell_ids, cell_ids)
  expect_equal(length(wr$milestone_ids), 4)
  expect_equal(nrow(wr$milestone_network), 3)
  expect_equal(nrow(wr$progressions), length(cell_ids))

  expect_equal(wr$milestone_ids, paste0("ML_", c("A", "D", "E", "F")))

  test_strs <- wr$milestone_network %>% {paste(.$from, .$to, .$length, .$directed, sep = "|")} %>% sort
  expected_strs <- c(
    "ML_A|ML_D|1.8|FALSE",
    "ML_D|ML_E|0.8|FALSE",
    "ML_D|ML_F|0.9|FALSE"
  ) %>% sort
  expect_equal(test_strs, expected_strs)

  test_strs <- wr$progressions %>% {paste(.$cell_id, .$from, .$to, round(.$percentage, 2), sep = "|")} %>% sort
  expected_strs <- c(
    "A|ML_A|ML_D|0",
    "B|ML_A|ML_D|0.28",
    "C|ML_A|ML_D|0.61",
    "D|ML_D|ML_E|0",
    "E|ML_D|ML_E|1",
    "F|ML_D|ML_F|1",
    "a|ML_A|ML_D|0",
    "b|ML_A|ML_D|0.28",
    "bb|ML_A|ML_D|0.28",
    "c|ML_A|ML_D|0.61",
    "cc|ML_A|ML_D|0.61",
    "d|ML_D|ML_E|0"
  ) %>% sort
  expect_equal(test_strs, expected_strs)
})
