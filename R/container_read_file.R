#' @importFrom readr read_lines
.container_read_file <- function(
  image,
  config = container_config(),
  path_container
) {
  temp_folder <- safe_tempdir("")
  on.exit(unlink(temp_folder, recursive = TRUE, force = TRUE))

  path_local <- paste0(temp_folder, "/tmpfile")

  .container_copy_file(
    image = image,
    config = config,
    path_container = path_container,
    path_local = path_local
  )

  readr::read_lines(path_local)
}
