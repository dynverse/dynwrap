#' Backend configuration for containerisation
#'
#' @param type Which backend to use. Can be either `"docker"` or `"singularity"`.
#'   If the value of this parameter is not passed explicitly, the default depend on
#'   the `"dynwrap_run_environment"` option, the `"DYNWRAP_RUN_ENVIRONMENT"` environment
#'   variable, or otherwise just `"docker"`.
#' @param ... Parameters to pass to `container_create_docker_config()` or `container_create_singularity_config()`.
#'
#' @usage
#' container_create_config(
#'   type =
#'     getOption("dynwrap_run_environment") \%||\%
#'     get_env_or_null("DYNWRAP_RUN_ENVIRONMENT") \%||\%
#'     "docker",
#'   ...
#' )
#'
#' @examples
#' \dontrun{
#' config <- container_create_docker_config()
#' container_set_default_config(config)
#'
#' config <- container_create_singularity_config(
#'   images_folder = "~/dynwrap_singularity_images/"
#' )
#' container_set_default_config(config)
#' }
#'
#' @export
container_create_config <- function(
  type = getOption("dynwrap_run_environment") %||% get_env_or_null("DYNWRAP_RUN_ENVIRONMENT") %||% "docker",
  ...
) {

  if (!type %in% c("docker", "singularity")) {
    stop("Container type must be either \"docker\" or \"singularity\"")
  }

  switch(
    type,
    docker = container_create_docker_config(...),
    singularity = container_create_singularity_config(...)
  )
}

#' @rdname container_create_config
#' @export
container_create_docker_config <- function() {
  list(type = "docker") %>% dynutils::add_class("dynwrap::container_config")
}

#' @param prebuild If the singularity images are not prebuilt, they will need to be built every time a method is run.
#' @param images_folder A folder in which to store the singularity images. Each TI method will require about 1GB of space.
#'
#' @usage
#' container_create_singularity_config(
#'   images_folder =
#'     getOption("dynwrap_singularity_images_folder") \%||\%
#'     get_env_or_null("DYNWRAP_SINGULARITY_IMAGES_FOLDER") \%||\%
#'     "./",
#'   prebuild = images_folder != "./"
#' )
#'
#' @rdname container_create_config
#' @export
container_create_singularity_config <- function(
  images_folder = getOption("dynwrap_singularity_images_folder") %||% get_env_or_null("DYNWRAP_SINGULARITY_IMAGES_FOLDER") %||% "./",
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



.container_config_file <- function() {
  if (.Platform$OS.type == "unix") {
    "~/.local/share/dynwrap/dynwrap_config.rds"
  } else if (.Platform$OS.type == "windows") {
    "~/../AppData/Local/dynwrap/dynwrap_config.rds"
  }
}

#' @rdname container_create_config
#'
#' @export
container_get_default_config <- function() {
  getOption("dynwrap_config") %||%
    read_rds_or_null(.container_config_file()) %||%
    container_create_config()
}

#' @rdname container_create_config
#'
#' @param config A config to save as default.
#' @param permanent Whether or not to save the config file permanently
#'
#' @importFrom readr write_rds
#'
#' @export
container_set_default_config <- function(config, permanent = TRUE) {
  if (permanent) {
    config_file <- .container_config_file()

    folder <- gsub("/[^/]*$", "", config_file)

    if (!file.exists(folder)) dir.create(folder, recursive = TRUE)

    readr::write_rds(config, path = config_file)
  } else {
    options(dynwrap_config = config)
  }
}
