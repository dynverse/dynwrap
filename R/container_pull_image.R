#' @importFrom jsonlite write_json
.container_pull_image <- function(
  image,
  config = container_config()
) {

  if (config$type == "docker") {
    processx::run("docker", c("pull", image), echo = TRUE)

  } else if (config$type == "singularity") {
    image_location <- .container_singularity_path(config, image)
    image_folder <- dirname(image_location)
    image_file <- basename(image_location)

    # create directory if not present yet
    dir.create(image_folder, recursive = TRUE, showWarnings = FALSE)

    tempcache <- .container_singularity_create_concurrent_cache(cachedir = image_folder)
    on.exit(.container_singularity_finalise_concurrent_cache(tempcache, cachedir = image_folder))

    env <- c(
      "SINGULARITY_PULL_FOLDER" = tempcache,
      "SINGULARITY_CACHEDIR" = tempcache
    )

    # pull container
    processx::run("singularity", c("pull", "--name", image_file, paste0("shub://", image)), echo = TRUE, env = env)
  }
}
