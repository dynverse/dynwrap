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
#' map_chr(tags, ~ dynwrap:::.container_get_digests(paste0("dynverse/dynwrap_tester:", .))$repo_digests) %>% set_names(tags) %>% deparse() %>% str_replace("^c\\(", "c(\n") %>% paste(collapse = "\n") %>% cat

dynwrap_repo_digests <- c(
  R_text = "dynverse/dynwrap_tester@sha256:ad07aa736d8c883ae178943f117e1cfd5f6fc9397408a035986b882cf6223e17",
  R_hdf5 = "dynverse/dynwrap_tester@sha256:cbaaf6e00e7df0ad239b33c43645d584d30670a6e6be04baa0615dea488b3689",
  R_rds = "dynverse/dynwrap_tester@sha256:23c049245e9ef5dd9b760da6a5a94a2f3dbf2b598d21b0485fa5ac338aaab6bd",
  R_dynwrap = "dynverse/dynwrap_tester@sha256:6130f4270dfd0bd0d9333422cc94041a62325cdffbec5a9fed34d2ff000899d9",
  R_feather = "dynverse/dynwrap_tester@sha256:81753038bbcbf16f45604d4623ea229a9ad9036116175de659e8e76ccf535b0e",
  python_hdf5 = "dynverse/dynwrap_tester@sha256:56ca0de1630fadbaa7bb7ec80767734bb75d87333f95b09ba008bd43e98c8177",
  python_text = "dynverse/dynwrap_tester@sha256:3c0e6c3fbf77d6cf78e26b2f0771c7fc2352a2d3f0b615466a3aea432b891435",
  python_feather = "dynverse/dynwrap_tester@sha256:96e9b3dddac349c43934c4a248af4e6a4e20b1fbcd78b8844aadb025711e9645"
)

# get example dataset
data("example_dataset")
dataset <- example_dataset
dataset_na <- dataset
dataset_na$counts <- dataset_na$expression <- dataset$expression * NA

for (tag in tags) {
  test_that(paste0("Testing create_ti_method_with_container and infer_trajectory with ", tag), {

    method <- create_ti_method_with_container(image = dynwrap_repo_digests[[tag]])

    definition <- method()

    expect_true(definition$id == paste0("dynwrap_tester_", tag))
    expect_is(definition$run_fun, "function")

    # this doesn't work on travis, for some reason.
    if (Sys.info()[["user"]] %in% maintainer_usernames) {
      expect_match(definition$repo_digests, gsub(".*@", "", dynwrap_repo_digests[[tag]]))
      expect_match(definition$repo_digests, gsub(":.*", "", dynwrap_repo_digests[[tag]]))
    }

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
