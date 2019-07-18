context("Testing calculate_average_by_group")

x <- matrix(seq_len(100), ncol = 10)
rownames(x) <- paste0("C", seq_len(nrow(x)))
colnames(x) <- paste0("G", seq_len(ncol(x)))
cell_grouping <- tibble(
  cell_id = rownames(x),
  group_id = c(rep("M1", 5), rep("M2", 5))
)

group_ids <- c("M1", "M2")

test_that("Testing function", {
  x_grouped <- calculate_average_by_group(x, cell_grouping)
  expect_equal(nrow(x_grouped), 2)
  expect_equal(ncol(x_grouped), 10)
  expect_equal(rownames(x_grouped), group_ids)
  expect_equal(colnames(x_grouped), colnames(x))
  expect_equal(as.vector(x_grouped), as.vector(rbind(colMeans(x[1:5,]), colMeans(x[6:10,]))))
})

test_that("Testing edge cases", {
  x_grouped <- calculate_average_by_group(x["C1", , drop = FALSE], cell_grouping %>% filter(cell_id == "C1"))
  expect_equal(nrow(x_grouped), 1)
  expect_equal(ncol(x_grouped), 10)
  expect_equal(rownames(x_grouped), group_ids[[1]])
  expect_equal(colnames(x_grouped), colnames(x))

  x_grouped <- calculate_average_by_group(x[, "G1", drop = FALSE], cell_grouping)
  expect_equal(nrow(x_grouped), 2)
  expect_equal(ncol(x_grouped), 1)
  expect_equal(rownames(x_grouped), group_ids)
  expect_equal(colnames(x_grouped), colnames(x)[[1]])

  x_grouped <- calculate_average_by_group(x, tibble(cell_id = rownames(x), group_id = "M1"))
  expect_equal(nrow(x_grouped), 1)
  expect_equal(ncol(x_grouped), 10)
  expect_equal(rownames(x_grouped), "M1")
  expect_equal(colnames(x_grouped), colnames(x))

  x_grouped <- calculate_average_by_group(x["C1", "G1", drop = FALSE], tibble(cell_id = "C1", group_id = "M1"))
  expect_equal(nrow(x_grouped), 1)
  expect_equal(ncol(x_grouped), 1)
  expect_equal(rownames(x_grouped), "M1")
  expect_equal(colnames(x_grouped), "G1")
})
