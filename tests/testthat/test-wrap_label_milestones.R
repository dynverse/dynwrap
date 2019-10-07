context("Testing add_trajectory")

id <- "a"
cell_ids <- c("truth", "universally", "acknowledged", "that", "a", "single")

milestone_ids <-  c("one", "two")
milestone_network <- tribble(
  ~from, ~to, ~length, ~directed,
  "one", "two", 1, TRUE
)
progressions <- tibble(
  cell_id = cell_ids,
  from = "one",
  to = "two",
  percentage = seq(0, 1, length.out = length(cell_ids))
)

expression <- matrix(
  c(
    progressions$percentage,
    1-progressions$percentage
  ),
  nrow = length(cell_ids),
  dimnames = list(cell_ids, c("G1", "G2"))
)


wr_orig <- wrap_data(
  id = id,
  cell_ids = cell_ids
) %>%
  add_trajectory(milestone_network = milestone_network, progressions = progressions)


test_that("Testing milestone labelling manually", {
  wr <- wr_orig %>% label_milestones(
    labelling = c(
      "one" = "end",
      "two" = "begin"
    )
  )

  milestone_labelling <- get_milestone_labelling(wr)

  expect_true(milestone_labelling[["one"]] == "end")
  expect_true(milestone_labelling[["two"]] == "begin")

  expect_error(label_milestones(wr_orig))
  expect_error(label_milestones(wr_orig, labelling = "yabbadabbadoo"))
  expect_error(label_milestones(wr_orig, labelling = TRUE))
})

test_that("Testing milestone labelling with expression", {
  markers <- list(
    "begin" = "G1",
    "end" = "G2"
  )
  wr <- wr_orig %>% label_milestones_markers(
    markers = markers,
    expression_source = expression,
    n_nearest_cells = 2
  )

  milestone_labelling <- get_milestone_labelling(wr)

  expect_true(milestone_labelling[["one"]] == "end")
  expect_true(milestone_labelling[["two"]] == "begin")
  expect_setequal(names(milestone_labelling), milestone_ids)

  # warning when multiple labels are mapped to the same milestone
  expect_warning(
    wr_orig %>% label_milestones_markers(
      markers = list(begin = "G1"),
      expression_source = expression,
      n_nearest_cells = 20
    )
  )
})

test_that("Testing get milestone labelling with expression", {
  expect_true(all(get_milestone_labelling(wr_orig) == wr_orig$milestone_ids))
  expect_true(get_milestone_labelling(wr_orig, label_milestones = c("one" = "begin"))["one"] == "begin")
  expect_error(get_milestone_labelling(wr_orig, label_milestones = c("wow" = "so much wow")))
})
