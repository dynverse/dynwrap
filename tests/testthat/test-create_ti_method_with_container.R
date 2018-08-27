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
  R_text = "dynverse/dynwrap_tester@sha256:dbd95ba1212ddfac227fa138e3c5b3d697cbc9a3abba380863807abc7b400d2e",
  python_text = "dynverse/dynwrap_tester@sha256:a0015b8fe9a08854adef747bd92123f5b5af22c70401fd9bee2727fe3df6f0ce",
  R_hdf5 = "dynverse/dynwrap_tester@sha256:c2e4522aa0795e154b3ee5ffda8968e4f6d598aacf1da698db851f66112dd119",
  python_hdf5 = "dynverse/dynwrap_tester@sha256:365101f1a6bcba50ed43a9f859745330c69caf054d23f86b6cabc9b87670fad8",
  R_rds = "dynverse/dynwrap_tester@sha256:86c25c9e93417064bc5c1c2ebd2c193026aed16448a6fc1160ada7fdee88345a",
  R_dynwrap = "dynverse/dynwrap_tester@sha256:290bdb2705353e93e4ddcd80aa7f143225473721d8bdacb03d071438691fb82b",
  R_feather = "dynverse/dynwrap_tester@sha256:a0861f1dca8dd4a3d25ed9f56fe22b5f506c0e36ab0b2532ac44d217261ad815",
  python_feather = "dynverse/dynwrap_tester@sha256:0cdf4400df40fa669e136f28382952ec0fe1407d1ec7cfbe30e4b0a5be0ba7f8"
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
