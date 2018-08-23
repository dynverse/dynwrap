#' @importFrom crayon bold
.container_run <- function(
  image,
  dir_dynwrap,
  debug,
  verbose,
  config = container_config()
) {
  image_name <- gsub("[:@].*$", "", image)
  image_location <- normalizePath(paste0(config$images_folder, "/", image_name, ".simg"), mustWork = FALSE)

  if (debug) {
    command <-
      if (config$type == "docker") {
        paste0("docker run --entrypoint 'bash' -e TMPDIR=/ti/tmp --workdir /ti/workspace -it -v ", dir_dynwrap, ":/ti ", image)
      } else if (config$type == "singularity") {
        paste0("SINGULARITYENV_TMPDIR=/ti/tmp singularity exec --cleanenv --pwd /ti/workspace -B ", dir_dynwrap, ":/ti ", image_location, " bash")
      }

    stop("Use this command for debugging: \n", crayon::bold(command), call. = FALSE)
  }


  if (config$type == "docker") {
    process <- processx::run(
      "docker",
      c("run", "-e", "TMPDIR=/ti/tmp", "--workdir", "/ti/workspace", "-v", paste0(dir_dynwrap, ":/ti"), image),
      echo = verbose,
      echo_cmd = verbose,
      spinner = TRUE,
      error_on_status = FALSE
    )

    if (process$status != 0 && !verbose) {
      cat(process$stderr)
      stop(call. = FALSE)
    }

    process
  } else if (config$type == "singularity") {
    if (!file.exists(image_location)) {
      stop(image_location, " not found!")
    }

    stdout <- stderr <- if (verbose) {""} else {FALSE}

    # use system2 here instead of processx
    # processx has a strange bug that it doesn't show any output of the method
    # probably some problem with buffering and singularity
    stdout_file <- tempfile()
    output <- system2(
      "singularity",
      c("-s", "run", "--cleanenv", "--pwd", "/ti/workspace", "-B", paste0(dir_dynwrap, ":/ti"), image_location),
      stdout = stdout_file,
      stderr = stdout_file,
      env = "SINGULARITYENV_TMPDIR=/ti/tmp"
    )

    cat(paste0(readLines(stdout_file), collapse = "\n"))

    if (output > 0) {
      stop(call. = FALSE)
    }

    list(
      status = output
    )
  }
}
