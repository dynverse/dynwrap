.container_get_remote_digests <- function(
  image,
  container_type,
  singularity_images_folder = .container_get_singularity_images_folder(container_type)
) {
  image <- gsub("@sha256:.*", "", image)

  if (container_type == "docker") {
    # check whether image is available locally
    result <- processx::run("docker", c("inspect", "--type=image", image, "--format='{{.RepoDigests}}'"), error_on_status = FALSE)

    if (result$status > 0) {
      NA
    } else {
      result$stdout %>%
        stringr::str_replace_all("^.*\\[", "") %>%
        stringr::str_replace_all("\\].*\n$", "") %>%
        stringr::str_split(",") %>%
        first()
    }
  } else if (container_type == "singularity") {
    simg_location <- normalizePath(paste0(singularity_images_folder, "/", image, ".simg"), mustWork = FALSE)
    json_location <- normalizePath(paste0(singularity_images_folder, "/", image, ".json"), mustWork = FALSE)

    if (!file.exists(simg_location)) {
      NA
    } else {
      if (!file.exists(json_location)) {
        c()
      } else {
        jsonlite::read_json(json_location, simplifyVector = TRUE)$repo_digests
      }
    }
  }
}
