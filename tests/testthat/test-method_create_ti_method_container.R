context("Testing create_ti_method_container")

skip_on_appveyor()
skip_on_os("mac")
skip_on_cran()

tags <- c("dynwrapr_tester:v0.1.0") # "dynwrappy3_tester:v0.1.0"

# get example dataset
data("example_dataset")
dataset <- example_dataset
dataset_na <- dataset
dataset_na$counts <- dataset_na$expression <- dataset$expression * NA

for (tag in tags) {
  test_that(paste0("Testing create_ti_method_container and infer_trajectory with ", tag), {
    wanted_version <- tester_versions[[tag]]

    container_id <- paste0("dynverse/", tag)
    method <- create_ti_method_container(container_id = container_id, return_function = FALSE)

    expect_equal(method$run$backend, "container")
    expect_equal(method$run$container_id, container_id)

    model0 <- infer_trajectory(dataset, method, parameters = list())
    expect_true(is_wrapper_with_trajectory(model0))

    expect_output({
      model1 <- infer_trajectory(dataset, method, parameters = list(verbose = TRUE), verbose = TRUE)
      expect_true(is_wrapper_with_trajectory(model1))
    })

    expect_output(expect_error(infer_trajectory(dataset, method, debug = TRUE)), regexp = "Error traceback")

    expect_output(
      expect_error(
        infer_trajectory(dataset_na, method)
      ),
      regexp = "missing values in|contains NaN"
    )
  })
}

