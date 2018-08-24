fix_windows_path <- function(path) {
  path <- path %>% gsub("\\\\", "/", .)

  start <- path %>% gsub("^([a-zA-Z]*):/.*", "/\\1", .) %>% tolower

  path %>% gsub("[^:/]:", start, .)
}

#' @importFrom crayon bold
.container_run <- function(
  image,
  dir_dynwrap,
  debug,
  verbose,
  config = container_config()
) {
  command <- match.arg(config$type, choices = c("docker", "singularity"))

  ti_run_sh <- "/ti/run.sh"

  # fix for windows computers
  dir_dynwrap <- fix_windows_path(dir_dynwrap)

  if (config$type == "docker") {
    # determine command arguments
    args <- c("run", "--entrypoint", ti_run_sh, "-e", "TMPDIR=/ti/tmp", "--workdir", "/ti/workspace", "-v", paste0(dir_dynwrap, ":/ti"), image)

    # docker requires no extra environment variables to be set
    env <- NULL

  } else if (config$type == "singularity") {
    tempcache <- on.exit(unlink(tempcache, recursive = TRUE))
    on.exit(.container_singularity_finalise_concurrent_cache(tempcache))

    # pull container directly from docker or use a prebuilt image
    if (config$prebuild) {
      image <- paste0("docker://", image)
    } else {
      image <- normalizePath(paste0(config$images_folder, "/", gsub("[:@].*$", ".simg", image)), mustWork = FALSE)
    }

    # determine command arguments
    args <- c("exec", ti_run_sh, "--cleanenv", "--pwd", "/ti/workspace", "-B", paste0(dir_dynwrap, ":/ti"), image)

    # tmpdir must be set to /ti/tmp
    env <- c("SINGULARITYENV_TMPDIR" = "/ti/tmp", "SINGULARITY_CACHEDIR" = tempcache)
  }

  if (debug) {
    # change entrypoint from /ti/run.sh to bash
    args[args == ti_run_sh] <- "bash"

    # simply print the command the user needs to use to enter the container
    command <-
      switch(
        config$type,
        docker = paste0(c(command, args, "-it"), collapse = " "),
        singularity = paste0(c(paste0(names(env), "=", env, collapse = " "), command, args), collapse = " ")
      )
    stop("Use this command for debugging: \n", crayon::bold(command), call. = FALSE)
  } else {
    # run container
    process <- processx::run(command, args, env = env, echo = verbose, echo_cmd = verbose, spinner = TRUE, error_on_status = FALSE)

    if (process$status != 0 && !verbose) {
      cat(process$stderr)
      stop(call. = FALSE)
    }

    process
  }
}
