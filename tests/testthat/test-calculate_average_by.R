context("Testing calculate_average_by_...")

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

test_that("Testing calculate_average_by_milestone_percentages", {
  x <- matrix(seq_len(110), ncol = 10)
  rownames(x) <- paste0("C", seq_len(nrow(x)))
  colnames(x) <- paste0("G", seq_len(ncol(x)))
  milestone_percentages <- data_frame(
    cell_id = rownames(x),
    z = seq(-1, 1, length.out = length(cell_id)),
    M1 = ifelse(z <= 0, -z, 0),
    M2 = 1 - abs(z),
    M3 = ifelse(z >= 0, z, 0)
  ) %>%
    select(-z) %>%
    gather(milestone_id, percentage, -cell_id)

  x_grouped <- calculate_average_by_milestone_percentages(x, milestone_percentages)
  expect_equal(nrow(x_grouped), 3)
  expect_equal(ncol(x_grouped), 10)
  expect_true(all(abs(diff(as.vector(x_grouped)) * 3 - 11) < 1e-10))
})
