skip_on_travis_mac <- function() {
  skip_if(Sys.getenv("TRAVIS") == "true" && tolower(Sys.info()[["sysname"]]) == 'mac')
}

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
  add_prior_information(start_id = cell_ids[[1]])

test_that("Testing create_docker_ti_method with compone", {
  skip_on_travis_mac()
  skip_on_appveyor()

  capture.output({
    method0 <- pull_docker_ti_method("dynverse/comp1")
  })
  expect_equal(method0()$short_name, "componentone")

  method1 <- create_docker_ti_method("dynverse/comp1")
  expect_equal(method1()$short_name, "componentone")

  # test with custom definition
  definition <- extract_definition_from_docker_image("dynverse/comp1")

  definition$name <- "test"
  method2 <- create_docker_ti_method("dynverse/comp1", definition)
  expect_true(method2()$name == "test")

  definition$input$required <- "whatever"
  expect_error(create_docker_ti_method("dynverse/comp1", definition))

  definition$output$required <- "whatever"
  expect_error(create_docker_ti_method("dynverse/comp1", definition))
})

tags <- c("R_text", "python_text", "R_hdf5", "python_hdf5", "R_rds", "R_dynwrap", "R_feather", "python_feather")
for (tag in tags) {
  test_that(paste0("Testing create_docker_ti_method and infer_trajectory with ", tag), {
    skip_on_appveyor()
    skip_on_travis_mac()
    skip_on_cran()

    if (!tag %in% c("python_feather")) {
      skip_on_travis() # only download one container on travis
    }

    capture.output({
      method <- pull_docker_ti_method(paste0("dynverse/comp1:", tag))()
      model <- infer_trajectory(task, method)
    })
    expect_true(is_wrapper_with_trajectory(model))
  })
}
