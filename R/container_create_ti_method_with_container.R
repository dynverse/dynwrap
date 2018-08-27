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

  current_repo_digest <- .container_get_digests(
    image = image,
    config = config
  )

  if (pull_if_needed && is.na(current_repo_digest)) {
    .container_pull_image(
      image = image,
      config = config
    )

    current_repo_digest <- .container_get_digests(
      image = image,
      config = config
    )
  }

  repo_digest <- if (grepl("@sha256:", image)) gsub(":[^@]*@", "@", image) else NULL

  ######################################################
  ####          PULL NEW IMAGE (IF NEEDED)          ####
  ######################################################

  if (config$type != "singularity" || config$prebuild) {
    image_not_found <- identical(current_repo_digest, NA)
    out_of_date <-
      !image_not_found && # lazy eval
      !is.null(repo_digest) &&
      (length(current_repo_digest$repo_digests) == 0 || !any(grepl(repo_digest, current_repo_digest$repo_digests)))

    if (image_not_found || out_of_date) {
      msg <- ifelse(image_not_found, "Image not found", "Local image is out of date")
      message("Pulling image: '", image, "'. Reason: '", msg, "'. This might take a while.")

      .container_pull_image(
        image = image,
        config = config
      )
    }
  }

  ######################################################
  ####              EXTRACT DEFINITION              ####
  ######################################################

  definition <- .container_get_definition(
    image = image,
    config = config
  )

  ######################################################
  ####               CHECK DEFINITION               ####
  ######################################################

  testthat::expect_true(is.character(definition$id))
  testthat::expect_true(is.character(definition$name))

  # TODO: Expand this, in case 3rd party containers are naughty

  ######################################################
  ####                CREATE RUN FUN                ####
  ######################################################

  definition$run_fun <- .container_make_run_fun(
    definition = definition,
    image = image
  )

  ######################################################
  ####      TRANSFORM DEFINITION TO TI METHOD       ####
  ######################################################

  method <- do.call(create_ti_method, definition)
}
