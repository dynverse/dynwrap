context("Testing get_output_processor")

test_that("get_output_processor works correctly", {
  op_trajectory <- get_output_processor("trajectory")
  expect_is(op_trajectory, "list")
  expect_equal(op_trajectory$processor, add_trajectory)
  expect_equal(sort(op_trajectory$required_args), c("milestone_network"))
  expect_equal(sort(op_trajectory$optional_args), c("allow_self_loops", "divergence_regions", "milestone_ids", "milestone_percentages", "progressions"))
  expect_equal(sort(op_trajectory$args), sort(c(op_trajectory$required_args, op_trajectory$optional_args)))

  op_expression <- get_output_processor("expression")
  expect_is(op_expression, "list")
  expect_equal(op_expression$processor, add_expression)
  expect_equal(sort(op_expression$required_args), c("counts", "expression"))
  expect_equal(sort(op_expression$optional_args), c("expression_projected", "feature_info"))
  expect_equal(sort(op_expression$args), sort(c(op_expression$required_args, op_expression$optional_args)))
})
