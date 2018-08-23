#' @importFrom jsonlite write_json
.container_pull_image <- function(
  image,
  container_type,
  singularity_images_folder = .container_get_singularity_images_folder(container_type)
) {
  if (container_type == "docker") {
    processx::run("docker", c("pull", image), echo = TRUE)
  } else if (container_type == "singularity") {
    repo_name <- gsub("@sha256:.*", "", image)
    repo_digest <- if (grepl("@sha256:", image)) image else NULL

    image_location <- normalizePath(paste0(singularity_images_folder, "/", repo_name, ".simg"), mustWork = FALSE)
    json_location <- normalizePath(paste0(singularity_images_folder, "/", repo_name, ".json"), mustWork = FALSE)

    dir.create(gsub("[^/]*$", "", image_location), showWarnings = FALSE, recursive = TRUE)

    if (file.exists(json_location)) file.remove(json_location)

    processx::run("singularity", c("build", image_location, glue("docker://{image}")), echo = TRUE)

    jsonlite::write_json(list(digest = NA, repo_digest = repo_digest), json_location)
  }
}
