#' @importFrom jsonlite write_json
.container_pull_image <- function(
  image,
  config = container_config()
) {

  if (config$type == "docker") {
    processx::run("docker", c("pull", image), echo = TRUE)

  } else if (config$type == "singularity") {
    image_location <- .container_singularity_path(config, image)

    tempcache <- .container_singularity_create_concurrent_cache()
    on.exit(.container_singularity_finalise_concurrent_cache(tempcache))

    env <- c(
      "SINGULARITY_CACHEDIR" = tempcache,
      "SINGULARITY_TMPDIR" = safe_tempdir("singularity_tmpdir"),
      "SINGULARITY_LOCALCACHEDIR" = safe_tempdir("singularity_localcachedir")
    )

    processx::run("singularity", c("pull", paste0("shub://", image), image_location), echo = TRUE, env = env)
  }
}
