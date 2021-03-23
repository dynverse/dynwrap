#' Create a TI method from a docker / singularity container
#'
#' These functions create a TI method from a container using `babelwhale`. Supports both docker and singularity as a backend. See [`vignette("create_ti_method_container", "dynwrap")`](../doc/create_ti_method_container.html) for a tutorial on how to create a containerized TI method.
#'
#' @param container_id The name of the container repository (e.g. `"dynverse/ti_angle"`).
#' @param pull_if_needed Pull the container if not yet available.
#' @inheritParams .method_process_definition
#'
#' @return A function that can be used to adapt the parameters of the method. This functions returns a list containing all metadata of the method, and can be used to [infer a trajectory][infer_trajectory()]
#'
#' @seealso vignette("create_ti_method_container", "dynwrap")
#'
#' @keywords create_ti_method
#'
#' @examples
#' library(babelwhale)
#'
#' # only run if docker works on this platform
#' if (test_docker_installation()) {
#'   method <- create_ti_method_container("dynverse/ti_angle")
#'   trajectory <- infer_trajectory(example_dataset, method())
#' }
#'
#' @importFrom babelwhale get_default_config pull_container test_docker_installation test_singularity_installation list_docker_images
#'
#' @export
create_ti_method_container <- function(
  container_id,
  pull_if_needed = TRUE,
  return_function = TRUE
) {
  config <- babelwhale::get_default_config()

  ######################################################
  ####           TEST DOCKER/SINGULARITY            ####
  ######################################################

  if (config$backend == "docker") {
    test_docker_installation()
  } else if (config$backend == "singularity") {
    test_singularity_installation()
  }

  ######################################################
  ####          PULL NEW IMAGE (IF NEEDED)          ####
  ######################################################

  if (config$backend == "docker") {
    tab <- list_docker_images(container_id)

    if (nrow(tab) == 0) {
      babelwhale::pull_container(container_id)
    }
  } else if (config$backend == "singularity") {
    babelwhale::pull_container(container_id)
  }

  ######################################################
  ####               CREATE DEFINITION              ####
  ######################################################

  definition <- .container_get_definition(container_id)

  # save container info
  definition$run <- list(
    backend = "container",
    container_id = container_id
  )

  .method_process_definition(definition = definition, return_function = return_function)
}




.method_execution_preproc_container <- function(method, inputs, priors, parameters, verbose, seed, debug) {
  dir_dynwrap <- dynutils::safe_tempdir("ti")

  # construct paths
  paths <- lst(
    dir_dynwrap,
    dir_workspace = file.path(dir_dynwrap, "workspace"),
    dir_tmp = file.path(dir_dynwrap, "tmp")
  )

  # create all subdirectories
  walk(paths, dir.create, showWarnings = FALSE, recursive = TRUE)

  task <- inputs
  task$priors <- priors
  task$parameters <- parameters
  task$verbose <- verbose
  task$seed <- seed

  # save data to file
  dynutils::write_h5(task, file.path(paths$dir_dynwrap, "input.h5"))

  # add extra information
  paths$prior_names <- names(priors)
  paths$debug <- debug
  paths$verbose <- verbose

  # return path information
  paths
}


.method_execution_execute_container <- function(method, preproc_meta) {
  # print information if desired
  if (preproc_meta$verbose) {
    cat("Input saved to ", preproc_meta$dir_dynwrap, "\n", sep = "")
    cat("Running method using babelwhale\n")
  }

  args <- c("--dataset", "/ti/input.h5", "--output", "/ti/output.h5")
  if (preproc_meta$debug) args <- c(args, "--debug")

  if (!is.null(preproc_meta$prior_names)) {
    # only the priors that were specified earlier
    # have been saved to a file, so specifying 'all' is ok
    args <- c(args, "--use_priors", "all")
  }

  # run container
  output <- babelwhale::run(
    container_id = method$run$container_id,
    command = NULL,
    args = args,
    volumes = paste0(preproc_meta$dir_dynwrap %>% fix_windows_path(), ":/ti"),
    workspace = "/ti/workspace",
    verbose = preproc_meta$verbose,
    debug = preproc_meta$debug
  )

  # print information if desired
  if (preproc_meta$verbose) {
    cat("Output saved to ", file.path(preproc_meta$dir_dynwrap, "output.h5"), "\n", sep = "")
    cat("Attempting to read in output with hdf5")
  }

  # return output
  dynutils::read_h5(file.path(preproc_meta$dir_dynwrap, "output.h5"))
}


.method_execution_postproc_container <- function(preproc_meta) {
  if (!preproc_meta$debug) {
    unlink(preproc_meta$dir_dynwrap, recursive = TRUE)
  }
}



fix_windows_path <- function(path) {
  path <- gsub("\\\\", "/", path)

  start <-
    gsub("^([a-zA-Z]):/.*", "/\\1", path) %>%
    tolower

  gsub("[^:/]:", start, path)
}
