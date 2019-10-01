context("Testing add_pseudotime")

id <- "a"
cell_ids <- c("truth", "universally", "acknowledged", "that", "a", "single")
extras <- list("man")

pseudotime <- c(0, .1, .4, .5, .8, 1) %>% set_names(cell_ids)

wr_orig <- wrap_data(
  id = id,
  cell_ids = cell_ids
)

test_that("add_pseudotime works as expected", {
  trajectory <- wr_orig %>% add_pseudotime(pseudotime = pseudotime)

  expect_equal(trajectory$pseudotime, pseudotime)

  expect_error(add_pseudotime(wr_orig, pseudotime = "whatever"))
})


