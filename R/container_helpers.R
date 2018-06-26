#' Tests whether docker is correctly installed and available
#'
#' @param detailed Whether top do a detailed check
#' @export
test_docker_installation <- function(detailed = FALSE) {
  if (!detailed) {
    tryCatch(
      {
        version <- suppressWarnings(system("docker version", intern = TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE))
        if (!is.null(attr(version, "status")) && attr(version, "status") == "0") stop()

        TRUE
      },
      error = function(e) FALSE
    )
  } else {
    # test if docker command is found
    tryCatch({
      version <- system("docker", intern = TRUE, ignore.stderr = TRUE)
      message(crayon::green("\u2714 Docker is installed"))
    },
    error = function(e) {
      installation_url <- switch(
        tolower(Sys.info()["sysname"]),
        windows = "https://docs.docker.com/docker-for-windows/install/",
        darwin = "https://docs.docker.com/docker-for-mac/install/",
        "https://docs.docker.com/install/"
      )

      stop(crayon::red("\u274C An installation of docker is necessary to run this method. See https://docs.docker.com/install/ for instructions."))
    })

    # test if docker daemon is running
    version <- suppressWarnings(system("docker version --format '{{.Client.APIVersion}}'", intern = TRUE)) %>%
      gsub("'", "", .) # remove trailing ' (in windows)
    if (!is.null(attr(version, "status")) && attr(version, "status") != "0") {
      stop(crayon::red(glue::glue("\u274C Docker daemon does not seem to be running... \n- Try running {crayon::bold('dockerd')} in the command line \n- See https://docs.docker.com/config/daemon/")))
    }
    message(crayon::green("\u2714 Docker daemon is running"))

    # test if docker version is recent enough
    if (utils::compareVersion("1.0", version) > 0) {
      stop(crayon::red("\u274C Docker API version is", version, ". Requires 1.0 or later"))
    }

    message(crayon::green(glue::glue("\u2714 Docker is at correct version (>1.0): ", version)))

    # test if docker format is linux
    ostype <- system("docker info --format '{{.OSType}}'", intern = TRUE) %>%
      gsub("'", "", .) # remove trailing ' (in windows)
    if (ostype != "linux") {
      stop(crayon::red(glue::glue("\u274C Docker is not running in linux mode, but in {ostype} mode. \n Please switch to linux containers: https://docs.docker.com/docker-for-windows/#switch-between-windows-and-linux-containers")))
    }

    message(crayon::green(glue::glue("\u2714 Docker is in linux mode")))

    # test if docker images can be pulled
    tryCatch({
      output <- system(glue::glue("docker pull alpine"), intern = TRUE, ignore.stderr = TRUE)
      if (!is.null(attr(output, "status")) && attr(output, "status") != "0") stop()

      message(crayon::green("\u2714 Docker can pull images"))
    },
    error = function(e) {
      print(output)
      stop(crayon::red("\u274C Unable to pull docker images."))
    })

    # test if docker can run images, will fail on windows if linux containers are not enabled
    tryCatch({
      system(glue::glue("docker run alpine"), intern = TRUE, ignore.stderr = TRUE)
      message(crayon::green("\u2714 Docker can run image"))
    },
    error = function(e) {
      stop(crayon::red("\u274C Unable to run an image"))
    })

    # test if docker volume can be mounted
    volume_dir <- mytempdir("")
    tryCatch({
      version <- system(glue::glue("docker run -v {volume_dir}:/mount alpine"), intern = TRUE, ignore.stderr = TRUE)
      message(crayon::green("\u2714 Docker can mount temporary volumes"))
    },
    error = function(e) {
      stop(crayon::red("\u274C Unable to mount temporary directory: {volume_dir}. \n\tOn windows, you need to enable the shared drives (https://rominirani.com/docker-on-windows-mounting-host-directories-d96f3f056a2c)"))
    })

    message(crayon::green(crayon::bold(stringr::str_pad("\u2714 Docker test succesful ", 90, side = "right", "-"))))

    TRUE
  }
}
