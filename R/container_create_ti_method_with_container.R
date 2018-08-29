#' Create a TI method from a docker image
#'
#' These functions create a TI method from a docker image. Supports both docker and singularity as a backend.
#'
#' @param image The name of the docker repository (e.g. `"dynverse/angle"`).
#'   It is recommended to include a specific version (e.g. `"dynverse/angle@sha256:473e54..."`).
#' @param config A container config. See [container_config()] for more information.
#' @param pull_if_needed Pull the image if not yet available.
#'
#' @export
create_ti_method_with_container <- function(
  image,
  version = NULL,
  config = container_config(),
  pull_if_needed = TRUE
) {
  ######################################################
  ####           TEST DOCKER/SINGULARITY            ####
  ######################################################

  if (config$type == "docker") {
    docker_installed <- test_docker_installation()
    if (!docker_installed) {
      test_docker_installation(detailed = TRUE)
    }
  } else if (config$type == "singularity") {
    # TODO: why is there no test_singularity_installation()?
  }

  ######################################################
  ####          FETCH CURRENT REPO DIGEST           ####
  ######################################################

  current_version <- .container_get_version(image, config)

  if (pull_if_needed && identical(current_version, NA)) {
    .container_pull_image(image, config)

    current_version <- .container_get_version(image, config)
  }

  ######################################################
  ####          PULL NEW IMAGE (IF NEEDED)          ####
  ######################################################

  if (config$type != "singularity" || config$prebuild) {
    if (identical(current_version, NA) || (!is.null(version) && current_version < version)) {
      msg <- ifelse(identical(current_version, NA), "Image not found", "Local image is out of date")
      message("Pulling image: '", image, "'. Reason: '", msg, "'. This might take a while.")

      .container_pull_image(
        image = image,
        config = config
      )

      new_version <- .container_get_version(image, config)
      if (!is.null(version) && new_version < version) {
        warning("After pulling '", image, "', version number is lower than requested.\nCurrent version: ", new_version, ", expected >= ", version)
      }
    }
  }

  ######################################################
  ####              EXTRACT DEFINITION              ####
  ######################################################

  definition <- .container_get_definition(image, config)

  ######################################################
  ####               CHECK DEFINITION               ####
  ######################################################

  testthat::expect_true(is.character(definition$id))
  testthat::expect_true(is.character(definition$name))

  # TODO: Expand this, in case 3rd party containers are naughty

  ######################################################
  ####                CREATE RUN FUN                ####
  ######################################################

  definition$run_fun <- .container_make_run_fun(definition, image)

  ######################################################
  ####      TRANSFORM DEFINITION TO TI METHOD       ####
  ######################################################

  method <- do.call(create_ti_method, definition)
}
