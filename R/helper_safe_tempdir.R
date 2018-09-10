# get mountable temporary directory
# on osx, the R temporary directory is placed in the /var folder, but this is not standard accessibale for docker
# in that case, we put it in /tmp
safe_tempdir <- function(subfolder) {
  dir <- file.path(tempfile(), subfolder) %>%
    fix_macosx_tmp()

  if (dir.exists(dir)) {
    unlink(dir, recursive = TRUE, force = TRUE)
  }

  dir.create(dir, recursive = TRUE)

  dir
}


fix_windows_path <- function(path) {
  path <- path %>% gsub("\\\\", "/", .)

  start <- path %>% sub("^([a-zA-Z]):/.*", "/\\1", .) %>% tolower

  path %>% sub("[^:/]:", start, .)
}

fix_macosx_tmp <- function(path) {
  gsub("^/var/", "/tmp/", path)
}



unfix_windows_path <- function(path) {
  path %>% gsub("^/c/", "C:/", .)
}
