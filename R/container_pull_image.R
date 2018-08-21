#' @importFrom jsonlite write_json
.container_pull_image <- function(
  image,
  image_type,
  repo_digest,
  singularity_images_folder = .container_get_singularity_images_folder(image_type)
) {
  if (!is.null(repo_digest)) {
    testthat::expect_match(repo_digest, "[a-zA-Z0-9]*/[a-zA-Z0-9]*@sha256:[a-zA-Z0-9]*")
    str <- repo_digest
  } else {
    str <- image
  }

  if (image_type == "docker") {
    processx::run("docker", c("pull", str), echo = TRUE)
  } else if (image_type == "singularity") {
    image_location <- normalizePath(paste0(singularity_images_folder, "/", repo_name, ".simg"), mustWork = FALSE)
    json_location <- normalizePath(paste0(singularity_images_folder, "/", repo_name, ".json"), mustWork = FALSE)

    dir.create(singularity_images_folder, showWarnings = FALSE, recursive = TRUE)

    if (file.exists(json_location)) file.remove(json_location)

    processx::run("singularity", c("build", image_location, glue("docker://{str}")), echo = TRUE)

    jsonlite::write_json(list(repo_digest = repo_digest), json_location)
  }
}
