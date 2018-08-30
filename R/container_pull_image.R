#' @importFrom jsonlite write_json
.container_pull_image <- function(
  image,
  config = container_config()
) {

  if (config$type == "docker") {
    processx::run("docker", c("pull", image), echo = TRUE)

  } else if (config$type == "singularity") {
    image_location <- .container_singularity_path(config, image)

    processx::run("singularity", c("pull", image_location, paste0("shub://", image)), echo = TRUE, env = env)
  }
}
