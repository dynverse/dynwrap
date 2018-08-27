.container_get_digests <- function(
  image,
  config = container_config()
) {
  image_name <- gsub("[:@].*$", "", image)

  if (config$type == "docker") {
    # check whether image is available locally
    result <- processx::run("docker", c("inspect", "--type=image", image, "--format='{{.Id}}\t{{.RepoDigests}}'"), error_on_status = FALSE)

    if (result$status > 0) {
      warning(paste0(result$stdout, "\n", result$stderr))
      NA
    } else {
      digest <- result$stdout %>%
        stringr::str_replace_all("\\t.*\n$", "") %>%
        stringr::str_replace_all("^'", "")
      repo_digests <-
        result$stdout %>%
        stringr::str_replace_all("^.*\\[", "") %>%
        stringr::str_replace_all("\\].*\n$", "") %>%
        stringr::str_split(" ") %>%
        first()
      lst(digest, repo_digests)
    }
  } else if (config$type == "singularity") {
    simg_location <- normalizePath(paste0(config$images_folder, "/", image_name, ".simg"), mustWork = FALSE)
    json_location <- normalizePath(paste0(config$images_folder, "/", image_name, ".json"), mustWork = FALSE)

    if (!file.exists(simg_location)) {
      NA
    } else {
      if (!file.exists(json_location)) {
        list(digest = "", repo_digests = "") # image is from previous version of dynwrap
      } else {
        jsonlite::read_json(json_location, simplifyVector = TRUE)[c("digest", "repo_digests")]
      }
    }
  }
}
