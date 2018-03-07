context("Cell grouping")

test_that("Testing group_counts", {
  counts <- matrix(rep(1, 100), ncol=10)
  rownames(counts) <- 1:10
  colnames(counts) <- 1:10
  cell_grouping <- tibble(cell_id=as.character(1:10), group_id=c(rep("1", 5), rep("2", 5)))

  counts_grouped <- group_counts(counts, cell_grouping)
  expect_equal(nrow(counts_grouped), 2)
  expect_equal(ncol(counts_grouped), 10)
  expect_true(all(counts_grouped==1))
})
