context("Testing create_ti_method_with_container")

skip_on_appveyor()
skip_on_os("mac")
skip_on_cran()

# only run all tags on maintainer platforms
maintainer_usernames <- c("rcannood", "wouters")

if (Sys.info()[["user"]] %in% maintainer_usernames) {
  tags <- c("R_text", "R_hdf5", "R_rds", "R_dynwrap", "R_feather", "python_hdf5", "python_text", "python_feather")
} else {
  tags <- "python_feather"
}

# specific dynwrap tester versions to test
# Obtained with:

#' @examples
#' options(dynwrap_run_environment = "docker")
#' map_chr(tags, ~ dynwrap:::.container_get_version(paste0("dynverse/dynwrap_tester:", .))) %>% set_names(tags) %>% deparse() %>% paste(collapse = "") %>% cat()

tester_versions <- c(R_text = "0.1.0.1", R_hdf5 = "0.1.0.1", R_rds = "0.1.0.1", R_dynwrap = "0.1.0.1", R_feather = "0.1.0.1", python_hdf5 = "0.1.0.1", python_text = "0.1.0.1", python_feather = "0.1.0.1")

# get example dataset
data("example_dataset")
dataset <- example_dataset
dataset_na <- dataset
dataset_na$counts <- dataset_na$expression <- dataset$expression * NA

for (tag in tags) {
  test_that(paste0("Testing create_ti_method_with_container and infer_trajectory with ", tag), {
    wanted_version <- tester_versions[[tag]]

    method <- create_ti_method_with_container(image = paste0("dynverse/dynwrap_tester:", tag), version = wanted_version)

    definition <- method()

    expect_true(definition$id == paste0("dynwrap_tester_", tag))
    expect_is(definition$run_fun, "function")

    expect_true(definition$version >= wanted_version)

    model0 <- infer_trajectory(dataset, definition, parameters = list())
    expect_true(is_wrapper_with_trajectory(model0))

    expect_output({
      model1 <- infer_trajectory(dataset, definition, parameters = list(verbose = TRUE), verbose = TRUE)
      expect_true(is_wrapper_with_trajectory(model1))
    })

    expect_output(expect_error(infer_trajectory(dataset, definition, debug = TRUE)), regexp = "Error traceback")

    expect_output(
      expect_error(
        infer_trajectory(dataset_na, definition)
      ),
      regexp = "missing values in|contains NaN"
    )
  })
}
