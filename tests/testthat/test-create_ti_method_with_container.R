#' context("Testing create_ti_method_with_container")
#'
#' skip_on_appveyor()
#' skip_on_os("mac")
#' skip_on_cran()
#'
#' # only run all tags on maintainer platforms
#' maintainer_usernames <- c("rcannood", "wouters")
#'
#' if (Sys.info()[["user"]] %in% maintainer_usernames) {
#'   tags <- c("R_text", "python_text", "R_hdf5", "python_hdf5", "R_rds", "R_dynwrap", "R_feather", "python_feather")
#' } else {
#'   tags <- "python_feather"
#' }
#'
#' # specific dynwrap tester versions to test
#' # Obtained with:
#'
#' #' @examples
#' #' map_chr(tags, ~ .container_get_digests(paste0("dynverse/dynwrap_tester:", .), container_type = "docker")$remote_digests) %>% set_names(tags) %>% deparse() %>% str_replace("^c\\(", "c(\n") %>% paste(collapse = "\n") %>% cat
#'
#' dynwrap_repo_digests <- c(
#'   R_text = "dynverse/dynwrap_tester:R_text@sha256:cf23c3162b0f883b623dc474f0a985beafc2b5bb44bcf32bee76e3fd92768c9c",
#'   python_text = "dynverse/dynwrap_tester:python_text@sha256:59fe17a280c9de9f05aa55571615cb74e133613b8dfd83efe0311620dcb34f8f",
#'   R_hdf5 = "dynverse/dynwrap_tester:R_hdf5@sha256:3160655bb37ffa36e4242caddd8e7b2d4296756892534fb20e97f8beec32b270",
#'   python_hdf5 = "dynverse/dynwrap_tester:python_hdf5@sha256:e1dbffbaea3928a90325ca3d6b060b6cfd1dfa083cbe704069e8a360581157e5",
#'   R_rds = "dynverse/dynwrap_tester:R_rds@sha256:7ed02baede27d91ad0a9458274932eb464e862161c630ae19b73312d0213f4e8",
#'   R_dynwrap = "dynverse/dynwrap_tester:R_dynwrap@sha256:9c5f1fc988a944fa312d775a731fb8300ba4feb6d7986dbe72b9c5b8990d6c06",
#'   R_feather = "dynverse/dynwrap_tester:R_feather@sha256:3e4370a4a9edde676f9ea0bd30cb4d5527f7bc28ba2fc9ab689e20c221bb6523",
#'   python_feather = "dynverse/dynwrap_tester:python_feather@sha256:5a43d9fd50c9347c5487f121dc7aa27fa7fb3131a7c800a38e8d6d769959cb45"
#' )
#'
#' # get example dataset
#' data("example_dataset")
#' dataset <- example_dataset
#' dataset_na <- dataset
#' dataset_na$counts <- dataset_na$expression <- dataset$expression * NA
#'
#' for (tag in tags) {
#'   test_that(paste0("Testing create_ti_method_with_container and infer_trajectory with ", tag), {
#'
#'     method <- create_ti_method_with_container(image = dynwrap_repo_digests[[tag]])
#'
#'     definition <- method()
#'
#'     expect_true(definition$id == paste0("dynwrap_tester_", tag))
#'     expect_is(definition$run_fun, "function")
#'     # expect_match(definition$remote_digests, gsub(".*@", "", dynwrap_repo_digests[[tag]]))
#'     # expect_match(definition$remote_digests, gsub(":.*", "", dynwrap_repo_digests[[tag]]))
#'     # ???
#'
#'     model0 <- infer_trajectory(dataset, definition, parameters = list())
#'     expect_true(is_wrapper_with_trajectory(model0))
#'
#'     expect_output({
#'       model1 <- infer_trajectory(dataset, definition, parameters = list(verbose = TRUE), verbose = TRUE)
#'       expect_true(is_wrapper_with_trajectory(model1))
#'     })
#'
#'     expect_output(expect_error(infer_trajectory(dataset, definition, debug = TRUE)), regexp = "Error traceback")
#'
#'     expect_output(
#'       expect_error(
#'         infer_trajectory(dataset_na, definition)
#'       ),
#'       regexp = "missing values in|contains NaN"
#'     )
#'   })
#' }
