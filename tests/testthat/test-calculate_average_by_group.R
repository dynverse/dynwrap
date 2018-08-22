context("Testing calculate_average_by_group")

test_that("Testing calculate_average_by_group", {
  x <- matrix(seq_len(100), ncol = 10)
  rownames(x) <- paste0("C", seq_len(nrow(x)))
  colnames(x) <- paste0("G", seq_len(ncol(x)))
  cell_grouping <- tibble(
    cell_id = rownames(x),
    group_id = c(rep("M1", 5), rep("M2", 5))
  )

  x_grouped <- calculate_average_by_group(x, cell_grouping)
  expect_equal(nrow(x_grouped), 2)
  expect_equal(ncol(x_grouped), 10)
  expect_equal(as.vector(x_grouped), as.vector(rbind(colMeans(x[1:5,]), colMeans(x[6:10,]))))
})
