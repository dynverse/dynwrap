#' @importFrom jsonlite write_json
.container_pull_image <- function(
  image,
  config = container_config()
) {

  if (config$type == "docker") {
    processx::run("docker", c("pull", image), echo = TRUE)

  } else if (config$type == "singularity") {
    tempcache <- .container_singularity_create_concurrent_cache()
    on.exit(.container_singularity_finalise_concurrent_cache(tempcache))

    if (config$prebuild) {
      image_name <- gsub("[.@].*$", "", image)
      repo_digests <- if (grepl("@sha256:", image)) image else NULL

      image_location <- normalizePath(paste0(config$images_folder, "/", image_name, ".simg"), mustWork = FALSE)
      json_location <- normalizePath(paste0(config$images_folder, "/", image_name, ".json"), mustWork = FALSE)

      dir.create(gsub("[^/]*$", "", image_location), showWarnings = FALSE, recursive = TRUE)

      if (file.exists(json_location)) file.remove(json_location)

      processx::run("singularity", c("build", image_location, paste0("docker://", image)), echo = TRUE, env = c("SINGULARITY_CACHEDIR" = tempcache))

      jsonlite::write_json(list(digest = NA, repo_digests = repo_digests), json_location)
    } else {
      processx::run("singularity", c("exec", paste0("docker://", image), "ls"), echo = TRUE, env = c("SINGULARITY_CACHEDIR" = tempcache))
    }

  }
}
