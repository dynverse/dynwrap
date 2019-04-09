context("Testing calculate_average_by_milestone_percentages")

x <- matrix(seq_len(110), ncol = 10)
rownames(x) <- paste0("C", seq_len(nrow(x)))
colnames(x) <- paste0("G", seq_len(ncol(x)))

milestone_ids <- c("M1", "M2", "M3")

milestone_percentages <- tibble(
  cell_id = rownames(x),
  z = seq(-1, 1, length.out = length(cell_id)),
  M1 = ifelse(z <= 0, -z, 0),
  M2 = 1 - abs(z),
  M3 = ifelse(z >= 0, z, 0)
) %>%
  select(-z) %>%
  gather(milestone_id, percentage, -cell_id)

test_that("Testing whether function works", {
  x_grouped <- calculate_average_by_milestone_percentages(x, milestone_percentages)
  expect_equal(nrow(x_grouped), 3)
  expect_equal(ncol(x_grouped), 10)
  expect_equal(rownames(x_grouped), milestone_ids)
  expect_equal(colnames(x_grouped), colnames(x))
  expect_true(all(abs(diff(as.vector(x_grouped)) * 3 - 11) < 1e-10))
})

test_that("Testing edge cases", {
  x_grouped <- calculate_average_by_milestone_percentages(x["C1", , drop = FALSE], milestone_percentages %>% filter(cell_id == "C1"))
  expect_equal(nrow(x_grouped), 3)
  expect_equal(ncol(x_grouped), 10)
  expect_equal(rownames(x_grouped), milestone_ids)
  expect_equal(colnames(x_grouped), colnames(x))

  x_grouped <- calculate_average_by_milestone_percentages(x[, "G1", drop = FALSE], milestone_percentages)
  expect_equal(nrow(x_grouped), 3)
  expect_equal(ncol(x_grouped), 1)
  expect_equal(rownames(x_grouped), milestone_ids)
  expect_equal(colnames(x_grouped), colnames(x)[[1]])

  x_grouped <- calculate_average_by_milestone_percentages(x, tibble(cell_id = rownames(x), milestone_id = "M1", percentage = 1))
  expect_equal(nrow(x_grouped), 1)
  expect_equal(ncol(x_grouped), 10)
  expect_equal(rownames(x_grouped), milestone_ids[[1]])
  expect_equal(colnames(x_grouped), colnames(x))

  x_grouped <- calculate_average_by_milestone_percentages(x["C1", "G1", drop = FALSE], tibble(cell_id = "C1", milestone_id = "M1", percentage = 1))
  expect_equal(nrow(x_grouped), 1)
  expect_equal(ncol(x_grouped), 1)
  expect_equal(rownames(x_grouped), milestone_ids[[1]])
  expect_equal(colnames(x_grouped), colnames(x)[[1]])
})
