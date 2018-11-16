#' Create a TI method from a docker / singularity container
#'
#' These functions create a TI method from a container using `babelwhale`. Supports both docker and singularity as a backend.
#'
#' @param container_id The name of the container repository (e.g. `"dynverse/ti_angle"`).
#' @param version The minimum required version of the TI method container.
#'   If the required version is higher than the currently installed version,
#'   the container will be pulled from dockerhub or singularityhub.
#' @param pull_if_needed Pull the container if not yet available.
#'
#' @importFrom babelwhale get_default_config pull_container
#'
#' @export
create_ti_container <- function(
  container_id,
  version = NULL,
  pull_if_needed = TRUE
) {
  config <- babelwhale::get_default_config()

  ######################################################
  ####           TEST DOCKER/SINGULARITY            ####
  ######################################################

  if (config$backend == "docker") {
    docker_installed <- test_docker_installation()
    if (!docker_installed) {
      test_docker_installation(detailed = TRUE)
    }
  } else if (config$backend == "singularity") {
    # TODO: there should be a test_singularity_installation()
  }

  ######################################################
  ####          FETCH CURRENT REPO DIGEST           ####
  ######################################################

  current_version <- .container_get_version(container_id)

  # pull if container can't be found
  if (pull_if_needed && identical(current_version, NA)) {
    babelwhale::pull_container(container_id)

    current_version <- .container_get_version(container_id)
  }

  ######################################################
  ####          PULL NEW IMAGE (IF NEEDED)          ####
  ######################################################

  if (config$backend != "singularity" || config$use_cache) {
    if (identical(current_version, NA) || (!is.null(version) && current_version < version)) {
      msg <- ifelse(identical(current_version, NA), "Container is not in cache", "Cache is out of date")
      message("Pulling container: '", container_id, "'. Reason: '", msg, "'. This might take a while.")

      babelwhale::pull_container(container_id)

      new_version <- .container_get_version(container_id)
      if (!is.null(version) && new_version < version) {
        warning("After pulling '", container_id, "', version number is lower than requested.\nCurrent version: ", new_version, ", expected >= ", version)
      }
    }
  }

  ######################################################
  ####               CREATE DEFINITION              ####
  ######################################################

  definition <- .container_get_definition(container_id)
  definition$run_info <- list(
    backend = "container",
    container_id = container_id
  )

  .method_process_definition(definition)
}
