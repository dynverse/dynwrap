
#' Create a TI method wrapper
#'
#' @param id A short name for the method, only lowercase characters allowed.
#' @param name The name of the TI method.
#' @param package_loaded The packages that need to be loaded before executing the method.
#' @param package_required The packages that need to be installed before executing the method.
#' @param parameters A list of parameters, which can be parsed using [parse_parameter_definition()].
#' @param run_fun A function to run the TI, needs to have 'counts' as its first param.
#' @param remotes_package Package from which the remote locations of dependencies have to be extracted, eg. `dynmethods`.
#' @param ... Other information about the wrapper, eg. apt_dependencies.
#'
#' @export
#'
#' @include method_parse_parameter_definition.R
create_ti_function <- function(
  id,
  name = id,
  parameters = NULL,
  run_fun,
  package_loaded = c(),
  package_required = c(),
  remotes_package = ifelse("dynmethods" %in% rownames(installed.packages()), "dynmethods", "dynwrap"),
  return_function = TRUE,
  ...
) {
  definition <- lst(
    id,
    name,
    parameters,
    run_info = lst(
      backend = "function",
      run_fun,
      package_loaded,
      package_required,
      remotes_package
    ),
    ...
  )

  .method_process_definition(definition, return_function = return_function)
}


.method_execution_preproc_function <- function(
  method
) {
  run_info <- method$run_info

  # initialise stdout/stderr files
  if (capture_output) {
    stdout_file <- tempfile()
    sink(stdout_file, type = "output")

    stderr_file <- tempfile()
    stderr_con <- file(stderr_file, open = "wt")
    sink(stderr_con, type = "message")
  }

  # create a temporary directory to set as working directory,
  # to avoid polluting the working directory if a method starts
  # producing files haphazardly
  tmp_dir <- tempfile(pattern = method$id)
  dir.create(tmp_dir)
  old_wd <- getwd()
  setwd(tmp_dir)

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

  lst(
    stdout_file,
    stderr_file,
    stderr_con,
    tmp_dir,
    old_wd
  )
}

.method_execution_execute_function <- function() {
  model <- do.call(method$run_info$run_fun, arglist)
}


.method_execution_postproc_function <- function(preproc_meta) {
  sink(type = "output")
  sink(type = "message")
  close(preproc_meta$stderr_con)

  if (capture_output) {
    stdout <- read_file(preproc_meta$stdout_file)
    stderr <- read_file(preproc_meta$stderr_file)
  } else {
    stdout <- ""
    stderr <- ""
  }

  # wd to previous folder
  setwd(preproc_meta$old_wd)

  # Remove temporary folder
  unlink(preproc_meta$tmp_dir, recursive = TRUE, force = TRUE)
}
