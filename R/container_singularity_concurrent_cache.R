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
