.container_get_version <- function(image) {
  config <- container_get_default_config()

  if (config$type == "docker") {
    # check whether image is available locally
    result <- processx::run("docker", c("inspect", "--type=image", image, "--format='{{ index .Config.Labels \"version\" }}'"), error_on_status = FALSE)

    if (result$status > 0) {
      warning(paste0(result$stdout, "\n", result$stderr))
      NA
    } else {
      result$stdout %>%
        stringr::str_replace_all("['\\n]", "")
    }
  } else if (config$type == "singularity") {
    simg_location <- .container_singularity_path(config, image)

    if (!file.exists(simg_location)) {
      NA
    } else {
      result <- processx::run("singularity", c("inspect", simg_location), error_on_status = FALSE)
      if (result$status > 0) {
        warning(paste0(result$stdout, "\n", result$stderr))
        NA
      } else {
        jsonlite::fromJSON(result$stdout, simplifyVector = TRUE)$VERSION
      }
    }
  }
}
