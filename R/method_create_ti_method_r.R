
#' Create a TI method wrapper
#'
#' @param run_fun A function to infer a trajectory, with parameters counts/expression, params, priors, verbose and seed
#' @param package_loaded The packages that need to be loaded before executing the method.
#' @param package_required The packages that need to be installed before executing the method.
#' @param remotes_package Package from which the remote locations of dependencies have to be extracted, eg. `dynmethods`.
#' @inheritParams .method_process_definition
#'
#' @export
create_ti_method_r <- function(
  definition,
  run_fun,
  package_required = character(),
  package_loaded = character(),
  remotes_package = character(),
  return_function = TRUE
) {
  definition <- .method_load_definition(definition)

  definition$run <- lst(
    backend = "function",
    run_fun,
    package_required,
    package_loaded,
    remotes_package
  )

  # further process the definition
  .method_process_definition(definition = definition, return_function = return_function)
}


.method_execution_preproc_function <- function(method) {
  run <- method$run

  # create a temporary directory to set as working directory,
  # to avoid polluting the working directory if a method starts
  # producing files haphazardly
  tmp_dir <- tempfile(pattern = method$method$id)
  dir.create(tmp_dir)
  old_wd <- getwd()
  setwd(tmp_dir)

  # Load required packages and namespaces
  if (!is.null(run$package_loaded) && !is.na(run$package_loaded) && length(run$package_loaded)) {
    for (pack in run$package_loaded) {
      suppressMessages(do.call(require, list(pack)))
    }
  }

  if (!is.null(run$package_required) && !is.na(run$package_required) && length(run$package_required)) {
    for (pack in run$package_required) {
      suppressMessages(do.call(requireNamespace, list(pack)))
    }
  }

  lst(
    tmp_dir,
    old_wd
  )
}

.method_execution_execute_function <- function(method, inputs, priors, parameters, verbose, seed, preproc_meta) {
  # combine inputs and parameters
  args <- c(
    inputs,
    lst(
      priors,
      params = parameters,
      verbose,
      seed
    )
  )

  # only give args that are requested by the function
  args <- args[intersect(names(args), names(formals(method$run$run_fun)))]

  model <- do.call(method$run$run_fun, args)

  model
}


.method_execution_postproc_function <- function(preproc_meta) {
  # wd to previous folder
  setwd(preproc_meta$old_wd)

  # Remove temporary folder
  unlink(preproc_meta$tmp_dir, recursive = TRUE, force = TRUE)
}
