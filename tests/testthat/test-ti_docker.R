context("Testing create_docker_ti_method")

id <- "a"
cell_ids <- c("truth", "universally", "acknowledged", "that", "a", "single")

num_features <- round(runif(1, 100, 120))
feature_names <- paste0("feature_", seq_len(num_features))

expression <- matrix(runif(num_features * length(cell_ids), 8, 12), nrow = length(cell_ids), dimnames = list(cell_ids, feature_names))
counts <- 2^expression - 1

task <-
  wrap_expression(
    id = id,
    expression,
    counts
  ) %>%
  add_prior_information(start_cells = cell_ids[[1]])

test_that("Testing create_docker_ti_method with compone", {
  sink("/dev/null")
  method0 <- pull_docker_ti_method("dynverse/comp1")
  sink()
  expect_equal(method0()$short_name, "componentone")

  method1 <- create_docker_ti_method("dynverse/comp1")
  expect_equal(method1()$short_name, "componentone")

  method2 <- create_docker_ti_method("dynverse/comp1", "test")
  expect_equal(method2()$short_name, "test")

  expect_error(create_docker_ti_method("dynverse/comp1", input_ids_required = "whatever"))
  expect_error(create_docker_ti_method("dynverse/comp1", output_ids = "whatever"))
})

tags <- c("R_text")
if (Sys.getenv("TRAVIS") != "true") {
  tags <- c(tags, c("R_hdf5", "python_hdf5", "R_rds", "R_dynwrap", "R_feather", "python_text"))
}
for (tag in tags) {
  test_that(paste0("Testing create_docker_ti_method and infer_trajectory with ", tag), {
    sink("/dev/null")
    method <- pull_docker_ti_method(paste0("dynverse/comp1:", tag))()
    infer_trajectory(task, method)
    sink()
  })
}

