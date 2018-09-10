
.container_singularity_path <- function(config, image) {
  path <-
    image %>%
    stringr::str_replace_all("@sha256:", "_hash-") %>%
    stringr::str_replace_all(":", "_tag-") %>%
    paste0(config$images_folder, ., ".simg") %>%
    normalizePath(mustWork = FALSE)

  dir.create(stringr::str_replace(path, "[^/]*.simg$", ""), recursive = TRUE, showWarnings = FALSE)

  path
}

#' @examples
#' config <- container_singularity(prebuild = TRUE, images_folder = "~/Workspace/dynverse/dynbenchmark/derived/03-method_characterisation/singularity_images/")
#' .container_singularity_path(config, "dynverse/dynwrap@sha256:3456789iuhijkm")
#' .container_singularity_path(config, "dynverse/dynwrap:r")
#' .container_singularity_path(config, "dynverse/dynwrap")
