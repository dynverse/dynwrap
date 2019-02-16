
#' Create a TI method wrapper
#'
#' @param id A short name for the method, only lowercase characters allowed.
#' @param name The name of the TI method.
#' @param package_loaded The packages that need to be loaded before executing the method.
#' @param package_required The packages that need to be installed before executing the method.
#' @param parameters A list of parameters.
#' @param run_fun A function to run the TI, needs to have 'counts' as its first param.
#' @param input_required The required inputs for this method. See `dynwrap::allowed_inputs()`.
#' @param input_optional Optional inputs for this method. See `dynwrap::allowed_inputs()`.
#' @param output The outputs produced by this method. See `dynwrap::allowed_outputs()`.
#' @param remotes_package Package from which the remote locations of dependencies have to be extracted, eg. `dynmethods`.
#' @param return_function Whether to return a function that allows you to override the default parameters, or just return the method meta data as is.
#' @param ... Other information about the wrapper, eg. apt_dependencies.
#'
#' @export
#'
#' @include method_parse_parameter_definition.R
create_ti_method_r <- function(
  id,
  name = id,
  parameters = NULL,
  run_fun,
  input_required,
  input_optional = NULL,
  output,
  package_loaded = c(),
  package_required = c(),
  remotes_package = ifelse("dynmethods" %in% rownames(installed.packages()), "dynmethods", "dynwrap"),
  return_function = TRUE,
  ...
) {
  # check that run_fun has the required arguments
  assert_that(
    c(input_required, input_optional, names(parameters$parameters), "verbose", "seed") %all_in% formalArgs(run_fun)
  )

  # process input and output vectors
  input <- list( # could be derived from the run_fn
    format = NA,
    required = input_required,
    optional = input_optional
  )
  output <- list( # this cannot
    format = NA,
    outputs = output
  )

  # create definition list
  definition <- lst(
    method = lst(
      id,
      name
    ),
    parameters,
    run = lst(
      backend = "function",
      run_fun,
      package_loaded,
      package_required,
      remotes_package
    ),
    input,
    output,
    ...
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
  if (!is.null(run$package_loaded) && !is.na(run$package_loaded)) {
    for (pack in run$package_loaded) {
      suppressMessages(do.call(require, list(pack)))
    }
  }

  if (!is.null(run$package_required) && !is.na(run$package_required)) {
    for (pack in run$package_required) {
      suppressMessages(do.call(requireNamespace, list(pack)))
    }
  }

  lst(
    tmp_dir,
    old_wd
  )
}

.method_execution_execute_function <- function(method, inputs, parameters, verbose, seed, preproc_meta) {
  # combine inputs and parameters
  args <- c(
    inputs,
    parameters,
    lst(verbose, seed)
  )

  # remove params that are not supposed to be here
  remove_args <- setdiff(names(args), formalArgs(method$run$run_fun))
  if (length(remove_args) > 0) {
    warning("Parameters [", paste(remove_args, collapse = ", "), "] not recognised by method; removing them from the arglist.")
    sel_args <- setdiff(names(args), remove_args)
    args <- args[remove_args]
  }

  model <- do.call(method$run$run_fun, args)

  model
}


.method_execution_postproc_function <- function(preproc_meta) {
  # wd to previous folder
  setwd(preproc_meta$old_wd)

  # Remove temporary folder
  unlink(preproc_meta$tmp_dir, recursive = TRUE, force = TRUE)
}
