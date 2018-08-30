#' @importFrom crayon bold
.container_run <- function(
  image,
  command,
  extra_args = NULL,
  debug = FALSE,
  verbose = FALSE,
  volumes = NULL,
  config = container_config(),
  workspace = NULL,
  environment_variables = NULL
) {
  container_cmd <- match.arg(config$type, choices = c("docker", "singularity"))

  # add safe tempdir to volumes
  safe_tmp <- safe_tempdir("tmp")
  on.exit(unlink(safe_tmp, recursive = TRUE))
  volumes <- c(volumes, paste0(safe_tmp, ":/tmp2"))

  if (config$type == "docker") {
    volumes <- unlist(map(volumes, ~ c("-v", .)))
  } else if (config$type == "singularity") {
    volumes <- c("-B", paste0(volumes, collapse = ","))
  }

  # check debug
  if (debug) {
    command <- "bash"
    extra_args <- NULL
  }

  # process workspace
  if (!is.null(workspace)) {
    if (config$type == "docker") {
      workspace <- c("--workdir", workspace)
    } else if (config$type == "singularity") {
      workspace <- c("--pwd", workspace)
    }
  }

  # process command
  if (config$type == "singularity") {
    sing_command <- "run"
  }

  if (!is.null(command)) {
    if (config$type == "docker") {
      command <- c("--entrypoint", command)
    } else if (config$type == "singularity") {
      sing_command <- "exec"
    }
  }

  # process environment variables
  environment_variables <- c(environment_variables, "TMPDIR=/tmp2")

  if (config$type == "docker") {
    env1 <- unlist(map(environment_variables, ~ c("-e", .)))
    env2 <- NULL

    # determine command arguments
    args <- c("run", command, env1, workspace, volumes, image, extra_args)

  } else if (config$type == "singularity") {
    tempcache <- .container_singularity_create_concurrent_cache()
    on.exit(.container_singularity_finalise_concurrent_cache(tempcache))

    env1 <- NULL
    env2 <- set_names(
      environment_variables %>% gsub("^(.*)=.*$", "SINGULARITYENV_\\1", .),
      environment_variables %>% gsub("^.*=", "", .)
    )
    env2 <- c(
      env2,
      "SINGULARITY_CACHEDIR" = tempcache,
      "SINGULARITY_TMPDIR" = safe_tempdir("singularity_tmpdir"),
      "SINGULARITY_LOCALCACHEDIR" = safe_tempdir("singularity_localcachedir")
    )

    # pull container directly from docker or use a prebuilt image
    if (!config$prebuild) {
      image <- paste0("docker://", image)
    } else {
      image < .container_singularity_path(config, image)
    }

    # determine command arguments
    args <- c(sing_command, workspace, volumes, image, command, extra_args)
  }

  if (debug) {
    # simply print the command the user needs to use to enter the container
    command <-
      switch(
        config$type,
        docker = paste0(c(container_cmd, args, "-it"), collapse = " "),
        singularity = paste0(c(paste0(names(env2), "=", env2, collapse = " "), container_cmd, args), collapse = " ")
      )
    stop("Use this command for debugging: \n", crayon::bold(command), call. = FALSE)
  } else {
    # run container
    process <- processx::run(container_cmd, args, env = env2, echo = verbose, echo_cmd = verbose, spinner = TRUE, error_on_status = FALSE)

    if (process$status != 0 && !verbose) {
      cat(process$stderr)
      stop(call. = FALSE)
    }

    process
  }
}
