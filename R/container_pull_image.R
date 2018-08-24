#' @importFrom jsonlite write_json
.container_pull_image <- function(
  image,
  config = container_config()
) {

  if (config$type == "docker") {
    processx::run("docker", c("pull", image), echo = TRUE)

  } else if (config$type == "singularity") {
    tempcache <- on.exit(unlink(tempcache, recursive = TRUE))
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
      processx::run("singularity", c("exec", "ls", paste0("docker://", image)), echo = TRUE, env = c("SINGULARITY_CACHEDIR" = tempcache))
    }

  }
}


.container_singularity_create_concurrent_cache <- function() {
  cachedir <- getenv("SINGULARITY_CACHEDIR") %||% paste0(getenv("HOME"), "/.singularity")
  tempcache <- safe_tempdir("tempcache")

  cached_files <- list.files(file.path(cachedir, "/docker"))
  walk(cached_files, function(file) {
    tempfile <- file.path(tempcache, "docker", file)
    cachedfile <- file.path(cachedir, "docker", file)
    file.symlink(cachedfile, tempfile)
  })
}

.container_singularity_finalise_concurrent_cache <- function(tempcache) {
  cachedir <- getenv("SINGULARITY_CACHEDIR") %||% paste0(getenv("HOME"), "/.singularity")

  new_files <- setdiff(list.files(file.path(tempcache, "/docker")), list.files(file.path(cachedir, "/docker")))
  walk(new_files, function(file) {
    tempfile <- file.path(tempcache, "docker", file)
    cachedfile <- file.path(cachedir, "docker", file)

        # just to make sure, check whether the new file is not a symbolic link
    # and whether it does not exist yet in the global cache before copying
    if (Sys.readlink(tempfile) == "" && !file.exists(cachedfile)) {
      file.copy(tempfile, cachedfile)
    }
  })

  unlink(tempcache, recursive = TRUE)
}
