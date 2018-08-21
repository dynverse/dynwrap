skip_on_travis_mac <- function() {
  skip_if(Sys.getenv("TRAVIS") == "true" && "darwin" %in% tolower(Sys.info()[["sysname"]]))
}

context("Testing create_ti_method_with_container")

skip_on_appveyor()
skip_on_travis_mac()
skip_on_cran()


if (Sys.getenv("TRAVIS") == "true") {
  tags <- "python_feather"
} else {
  tags <- c("latest", "R_text", "python_text", "R_hdf5", "python_hdf5", "R_rds", "R_dynwrap", "R_feather", "python_feather")
}
#' Obtained with:
#' @examples
#' map_chr(tags, ~ .container_get_remote_digests(paste0("dynverse/dynwrap_tester:", .), image_type = "docker")) %>% set_names(tags) %>% deparse() %>% cat

dynwrap_repo <- "dynverse/dynwrap_tester"
dynwrap_repo_digests <- c(
  R_text = "dynverse/dynwrap_tester@sha256:cf23c3162b0f883b623dc474f0a985beafc2b5bb44bcf32bee76e3fd92768c9c",
  python_text = "dynverse/dynwrap_tester@sha256:59fe17a280c9de9f05aa55571615cb74e133613b8dfd83efe0311620dcb34f8f",
  R_hdf5 = "dynverse/dynwrap_tester@sha256:a74072b68e1118f9857ffde2246d81914ed46c05658006b7ecf9a9bc431e19e7",
  python_hdf5 = "dynverse/dynwrap_tester@sha256:1d8c09ebfe6c418c6dae0790246f5f535ce3b70448648b58e5b16de0a119aba4",
  R_rds = "dynverse/dynwrap_tester@sha256:7ed02baede27d91ad0a9458274932eb464e862161c630ae19b73312d0213f4e8",
  R_dynwrap = "dynverse/dynwrap_tester@sha256:9c5f1fc988a944fa312d775a731fb8300ba4feb6d7986dbe72b9c5b8990d6c06",
  R_feather = "dynverse/dynwrap_tester@sha256:3e4370a4a9edde676f9ea0bd30cb4d5527f7bc28ba2fc9ab689e20c221bb6523",
  python_feather = "dynverse/dynwrap_tester@sha256:5a43d9fd50c9347c5487f121dc7aa27fa7fb3131a7c800a38e8d6d769959cb45"
)

singularity_images_folder <- safe_tempdir("singularity_images")
on.exit(unlink(singularity_images_folder, recursive = TRUE))

id <- "a"
cell_ids <- c("truth", "universally", "acknowledged", "that", "a", "single")

num_features <- round(runif(1, 100, 120))
feature_names <- paste0("feature_", seq_len(num_features))

expression <- matrix(runif(num_features * length(cell_ids), 8, 12), nrow = length(cell_ids), dimnames = list(cell_ids, feature_names))
counts <- 2^expression - 1

dataset <-
  wrap_expression(
    id = id,
    expression,
    counts
  ) %>%
  add_prior_information(start_id = cell_ids[[1]])


for (tag in tags) {
  test_that(paste0("Testing create_docker_ti_method and infer_trajectory with ", tag), {

    method <- create_ti_method_with_container(
      image = paste0("dynverse/dynwrap_tester:", tag),
      repo_digest = dynwrap_repo_digests[[tag]]
    )
    definition <- method()

    expect_true(definition$id == paste0("dynwrap_tester_", tag))
    expect_is(definition$run_fun, "function")
    expect_match(definition$remote_digests, dynwrap_repo_digests[[tag]])

    model <- infer_trajectory(dataset, definition, parameters = list(verbose = TRUE), verbose = T)
    expect_true(is_wrapper_with_trajectory(model))

    expect_error(infer_trajectory(dataset, method, debug = TRUE))
  })
}
