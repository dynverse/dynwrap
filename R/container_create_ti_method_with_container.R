#' Create a TI method from a docker image
#'
#' These functions create a TI method from a docker image. Supports both docker and singularity as a backend.
#'
#' @param image The name of the docker repository (e.g. `"dynverse/angle"`).
#'   It is recommended to include a specific version (e.g. `"dynverse/angle@sha256:473e54..."`).
#' @param container_type In which environment to run the method, can be `"docker"` or `"singularity"`.
#'   By default, docker will be used.
#' @param singularity_images_folder The location of the folder containing the singularity images.
#'   By default, this will use either the `DYNWRAP_SINGULARITY_IMAGES_FOLDER` environment variable,
#'   the `dynwrap_singularity_images_folder` option, or otherwise the working directory (not recommended).
#'
#' @export
create_ti_method_with_container <- function(
  image,
  container_type = getOption("dynwrap_run_environment"),
  singularity_images_folder = .container_get_singularity_images_folder(container_type)
) {
  ######################################################
  ####               CHECK ARGUMENTS                ####
  ######################################################
  # Check arguments
  if (is.null(container_type)) {
    container_type <- "docker"
  }
  if (!container_type %in% c("docker", "singularity")) {
    stop(sQuote("container_type"), " must be either \"docker\" or \"singularity\"")
  }

  ######################################################
  ####           TEST DOCKER/SINGULARITY            ####
  ######################################################

  if (container_type == "docker") {
    docker_installed <- test_docker_installation()
    if (!docker_installed) {
      test_docker_installation(detailed = TRUE)
    }
  } else if (container_type == "singularity") {
    # TODO: why is there no test_singularity_installation()?
  }

  ######################################################
  ####          FETCH CURRENT REPO DIGEST           ####
  ######################################################

  current_repo_digest <- .container_get_digests(
    image = image,
    container_type = container_type,
    singularity_images_folder = singularity_images_folder
  )

  repo_digest <- if (grepl("@sha256:", image)) gsub(":[^@]*@", "@", image) else NULL

  ######################################################
  ####          PULL NEW IMAGE (IF NEEDED)          ####
  ######################################################

  image_not_found <- identical(current_repo_digest, NA)
  out_of_date <-
    !is.null(repo_digest) &&
    (length(current_repo_digest$remote_digests) == 0 || !any(grepl(repo_digest, current_repo_digest$remote_digests)))

  if (image_not_found || out_of_date) {
    msg <- ifelse(image_not_found, "Image not found", "Local image is out of date")
    message("Pulling image: '", image, "'. Reason: '", msg, "'. This might take a while.")

    .container_pull_image(
      image = image,
      container_type = container_type,
      singularity_images_folder = singularity_images_folder
    )
  }

  ######################################################
  ####              EXTRACT DEFINITION              ####
  ######################################################

  definition <- .container_get_definition(
    image = image,
    container_type = container_type,
    singularity_images_folder = singularity_images_folder
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
    image = image,
    container_type = container_type,
    singularity_images_folder = singularity_images_folder
  )

  ######################################################
  ####      TRANSFORM DEFINITION TO TI METHOD       ####
  ######################################################

  method <- do.call(create_ti_method, definition)
}
