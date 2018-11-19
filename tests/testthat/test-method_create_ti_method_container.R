context("Testing create_ti_method_container")

skip_on_appveyor()
skip_on_os("mac")
skip_on_cran()

# only run all tags on maintainer platforms
maintainer_usernames <- c("rcannood", "wouters")

if (Sys.info()[["user"]] %in% maintainer_usernames) {
  tags <- c("R_text", "R_hdf5", "R_rds", "R_dynwrap", "python_hdf5", "python_text")
} else {
  tags <- "python_hdf5"
}

# specific dynwrap tester versions to test
# Obtained with:

#' @examples
#' map_chr(tags, ~ dynwrap:::.container_get_version(paste0("dynverse/dynwrap_tester:", .))) %>% set_names(tags) %>% deparse() %>% paste(collapse = "") %>% cat()

tester_versions <- c(R_text = "0.2.0.1", R_hdf5 = "0.2.0.1", R_rds = "0.2.0.1", R_dynwrap = "0.2.0.1", python_hdf5 = "0.2.0.1", python_text = "0.2.0.1")

# get example dataset
data("example_dataset")
dataset <- example_dataset
dataset_na <- dataset
dataset_na$counts <- dataset_na$expression <- dataset$expression * NA

for (tag in tags) {
  test_that(paste0("Testing create_ti_method_container and infer_trajectory with ", tag), {
    wanted_version <- tester_versions[[tag]]

    container_id <- paste0("dynverse/dynwrap_tester:", tag)
    method <- create_ti_method_container(container_id = container_id, version = wanted_version, return_function = FALSE)

    expect_true(method$id == paste0("dynwrap_tester_", tag))
    expect_equal(method$run_info$backend, "container")
    expect_equal(method$run_info$container_id, container_id)

    expect_true(method$version >= wanted_version)

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
