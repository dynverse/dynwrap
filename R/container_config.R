getenv <- function(x) {
  y <- Sys.getenv(x)
  if (y == "") y <- NULL
  y
}

#' Backend configuration for containerisation
#'
#' @param type Which backend to use. Can be either `"docker"` or `"singularity"`.
#'   If the value of this parameter is not passed explicitly, the default depend on
#'   the `"dynwrap_run_environment"` option, the `"DYNWRAP_RUN_ENVIRONMENT"` environment
#'   variable, or otherwise just `"docker"`.
#' @param ... Parameters to pass to `container_docker()` or `container_singularity()`.
#'
#' @export
container_config <- function(
  type = getOption("dynwrap_run_environment") %||% getenv("DYNWRAP_RUN_ENVIRONMENT") %||% "docker",
  ...
) {

  if (!type %in% c("docker", "singularity")) {
    stop("Container type must be either \"docker\" or \"singularity\"")
  }

  switch(
    type,
    docker = container_docker(...),
    singularity = container_singularity(...)
  )
}

#' @rdname container_config
#' @export
container_docker <- function() {
  list(type = "docker") %>% dynutils::add_class("dynwrap::container_config")
}

#' @param prebuild If the singularity images are not prebuilt, they will need to be built every time a method is run.
#' @param images_folder A folder in which to store the singularity images. Each TI method will require about 1 to 2GB of space.
#'
#' @rdname container_config
#' @export
container_singularity <- function(
  images_folder = getOption("dynwrap_singularity_images_folder") %||% getenv("DYNWRAP_SINGULARITY_IMAGES_FOLDER") %||% "./",
  prebuild = images_folder != "./"
) {
  if (prebuild && images_folder == "./") {
    warning(
      "No singularity images folder specified, will use the working directory.\n",
      "Check `?container_singularity` for more information on how to define the images folder."
    )
  }

  lst(
    type = "singularity",
    prebuild,
    images_folder
  ) %>%
    dynutils::add_class("dynwrap::container_config")
}
