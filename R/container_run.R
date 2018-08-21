#' @importFrom crayon bold
.container_run <- function(
  image,
  container_type,
  volumes,
  debug,
  verbose,
  singularity_images_folder
) {
  image_location <- normalizePath(paste0(singularity_images_folder, "/", image, ".simg"), mustWork = FALSE)

  if (debug) {
    if (container_type == "docker") {
      command <- glue::glue(
        "docker run --entrypoint 'bash' -e TMPDIR=/ti/tmp --workdir /ti/workspace -it {paste0(paste0('-v ', volumes), collapse = ' ')} {image}"
      )
    } else if (container_type == "singularity") {
      command <- glue::glue(
        "SINGULARITYENV_TMPDIR=/ti/tmp singularity exec --cleanenv --pwd /ti/workspace -B {glue::glue_collapse(volumes, ',')} {image_location} bash"
      )
    }

    stop("Use this command for debugging: \n", crayon::bold(command), call. = FALSE)
  }


  if (container_type == "docker") {
    process <- processx::run(
      "docker",
      c("run", "-e", "TMPDIR=/ti/tmp", "--workdir", "/ti/workspace", as.character(rbind("-v", volumes)), image),
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
  } else if (container_type == "singularity") {
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
      c("-s", "run", "--cleanenv", "--pwd", "/ti/workspace", "-B", glue::glue_collapse(volumes, ','), image_location),
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
