.method_execution_preproc_function <- function() {
  # initialise stdout/stderr files
  if (capture_output) {
    stdout_file <- tempfile()
    sink(stdout_file, type = "output")

    stderr_file <- tempfile()
    stderr_con <- file(stderr_file, open = "wt")
    sink(stderr_con, type = "message")

    run_info$stdout_file <- stdout_file
    run_info$stderr_file <- stderr_file
    run_info$stderr_con <- stderr_con
  }

  # create a temporary directory to set as working directory,
  # to avoid polluting the working directory if a method starts
  # producing files :angry_face:
  tmp_dir <- tempfile(pattern = method$id)
  dir.create(tmp_dir)
  old_wd <- getwd()
  setwd(tmp_dir)

  run_info$tmp_wd <- tmp_dir
  run_info$old_wd <- old_wd

  # Load required packages and namespaces
  if (!is.null(run_info$package_loaded) && !is.na(run_info$package_loaded)) {
    for (pack in run_info$package_loaded) {
      suppressMessages(do.call(require, list(pack)))
    }
  }

  if (!is.null(run_info$package_required) && !is.na(run_info$package_required)) {
    for (pack in run_info$package_required) {
      suppressMessages(do.call(requireNamespace, list(pack)))
    }
  }
}

.method_execution_execute_function <- function() {
  model <- do.call(method$run_fun, arglist)
}


.method_execution_postproc_function <- function() {
  sink(type = "output")
  sink(type = "message")
  close(run_info$stderr_con)

  if (capture_output) {
    stdout <- read_file(run_info$stdout_file)
    stderr <- read_file(run_info$stderr_file)
  } else {
    stdout <- ""
    stderr <- ""
  }

  # wd to previous folder
  setwd(run_info$old_wd)
  # Remove temporary folder
  unlink(run_info$tmp_wd, recursive = TRUE, force = TRUE)
}
