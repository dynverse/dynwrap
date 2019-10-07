context("Testing add_cell_graph")


# cell data
cell_ids <- c("A", "B", "C", "D", "E", "F", "G", "H", "a", "b", "bb", "c", "cc", "d")

wr_orig <- wrap_data(
  id = "test",
  cell_ids = cell_ids
)


test_that("Testing add_cell_graph", {
  cell_graph <- tibble::tribble(
    ~from, ~to, ~length, ~directed,
    "A", "B", .5, F,
    "B", "C", .6, F,
    "C", "D", .7, F,
    "D", "E", .8, F,
    "D", "F", .9, F,
    "E", "G", 0.5, F,
    "F", "G", 0.1, F,
    "G", "H", 1, F,
    "a", "A", .1, F,
    "b", "B", .1, F,
    "bb", "B", .08, F,
    "c", "C", .05, F,
    "cc", "c", .1, F,
    "d", "D", .01, F
  )
  to_keep <- c(A = T, B = T, C = T, D = T, E = T, "F" = T, G = T, H = T, a = F, b = F, bb = F, c = F, cc = F, d = F)

  wr <- wr_orig %>% add_cell_graph(
    cell_graph = cell_graph,
    to_keep = to_keep,
    milestone_prefix = "ML_"
  )

  # testing is_ti_data_wrapper
  expect_false(is_wrapper_with_trajectory(wr_orig))
  expect_true(is_wrapper_with_trajectory(wr))

  # testing milestone ids
  expect_equal(wr$cell_ids, cell_ids)
  expect_equal(length(wr$milestone_ids), 5)
  expect_equal(nrow(wr$milestone_network), 5)
  expect_equal(nrow(wr$progressions), length(cell_ids))

  expect_equal(wr$milestone_ids, paste0("ML_", c("A", "D", "F", "G", "H")))

  test_strs <- wr$milestone_network %>% {paste(.$from, .$to, .$length, .$directed, sep = "|")} %>% sort
  expected_strs <- c(
    "ML_A|ML_D|1.8|FALSE",
    "ML_D|ML_F|0.9|FALSE",
    "ML_D|ML_G|1.3|FALSE",
    "ML_F|ML_G|0.1|FALSE",
    "ML_G|ML_H|1|FALSE"
  ) %>% sort
  expect_equal(test_strs, expected_strs)

  test_strs <- wr$progressions %>% {paste(.$cell_id, .$from, .$to, round(.$percentage, 2), sep = "|")} %>% sort
  expected_strs <- c(
    'a|ML_A|ML_D|0',
    'A|ML_A|ML_D|0',
    'b|ML_A|ML_D|0.28',
    'B|ML_A|ML_D|0.28',
    'bb|ML_A|ML_D|0.28',
    'c|ML_A|ML_D|0.61',
    'C|ML_A|ML_D|0.61',
    'cc|ML_A|ML_D|0.61',
    'd|ML_A|ML_D|1',
    'D|ML_A|ML_D|1',
    'E|ML_D|ML_G|0.62',
    'F|ML_D|ML_F|1',
    'G|ML_D|ML_G|1',
    'H|ML_G|ML_H|1'
  ) %>% sort
  expect_equal(test_strs, expected_strs)
})
