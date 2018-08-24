context("Testing create_ti_method_with_container")

skip_on_appveyor()
skip_on_os("mac")
skip_on_cran()

# only run all tags on maintainer platforms
maintainer_usernames <- c("rcannood", "wouters")

if (Sys.info()[["user"]] %in% maintainer_usernames) {
  tags <- c("R_text", "python_text", "R_hdf5", "python_hdf5", "R_rds", "R_dynwrap", "R_feather", "python_feather")
} else {
  tags <- "python_feather"
}

# specific dynwrap tester versions to test
# Obtained with:

#' @examples
#' options(dynwrap_run_environment = "docker")
#' map_chr(tags, ~ dynwrap:::.container_get_digests(paste0("dynverse/dynwrap_tester:", .))$repo_digests) %>% set_names(tags) %>% deparse() %>% str_replace("^c\\(", "c(\n") %>% paste(collapse = "\n") %>% cat

dynwrap_repo_digests <- c(
  R_text = "dynverse/dynwrap_tester@sha256:d2b3950995eff97563981c9ce0ffefba6a654e45c90b8ab42f973d5602f58db6",
  python_text = "dynverse/dynwrap_tester@sha256:d2b3950995eff97563981c9ce0ffefba6a654e45c90b8ab42f973d5602f58db6",
  R_hdf5 = "dynverse/dynwrap_tester@sha256:d2b3950995eff97563981c9ce0ffefba6a654e45c90b8ab42f973d5602f58db6",
  python_hdf5 = "dynverse/dynwrap_tester@sha256:d2b3950995eff97563981c9ce0ffefba6a654e45c90b8ab42f973d5602f58db6",
  R_rds = "dynverse/dynwrap_tester@sha256:d2b3950995eff97563981c9ce0ffefba6a654e45c90b8ab42f973d5602f58db6",
  R_dynwrap = "dynverse/dynwrap_tester@sha256:d2b3950995eff97563981c9ce0ffefba6a654e45c90b8ab42f973d5602f58db6",
  R_feather = "dynverse/dynwrap_tester@sha256:d2b3950995eff97563981c9ce0ffefba6a654e45c90b8ab42f973d5602f58db6",
  python_feather = "dynverse/dynwrap_tester@sha256:d2b3950995eff97563981c9ce0ffefba6a654e45c90b8ab42f973d5602f58db6"
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
