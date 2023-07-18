#' Create a TI method from an R function wrapper
#'
#' @param run_fun A function to infer a trajectory, with parameters counts/expression, parameters, priors, verbose and seed
#' @param package_loaded The packages that need to be loaded before executing the method.
#' @param package_required The packages that need to be installed before executing the method.
#' @param remotes_package Package from which the remote locations of dependencies have to be extracted, eg. `dynmethods`.
#' @inheritParams .method_process_definition
#'
#' @inherit create_ti_method_container return
#'
#' @keywords create_ti_method
#' 
#' @return A method definition
#'
#' @examples
#' # define the parameters and other metadata
#' definition <- definition(
#'   method = def_method(
#'     id = "comp1"
#'   ),
#'   parameters = def_parameters(
#'     dynparam::integer_parameter(
#'       id = "component",
#'       default = 1,
#'       distribution = dynparam::uniform_distribution(1, 10),
#'       description = "The nth component to use"
#'     )
#'   ),
#'   wrapper = def_wrapper(
#'     input_required = "expression",
#'     input_optional = "start_id"
#'   )
#' )
#'
#' # define a wrapper function
#' run_fun <- function(expression, priors, parameters, seed, verbose) {
#'   pca <- prcomp(expression)
#'
#'   pseudotime <- pca$x[, parameters$component]
#'
#'   # flip pseudotimes using start_id
#'   if (!is.null(priors$start_id)) {
#'     if(mean(pseudotime[start_id]) > 0.5) {
#'      pseudotime <- 1-pseudotime
#'     }
#'   }
#'
#'   wrap_data(cell_ids = rownames(expression)) %>%
#'     add_linear_trajectory(pseudotime = pseudotime)
#' }
#'
#' method <- create_ti_method_r(definition, run_fun, package_loaded = "dplyr")
#' trajectory <- infer_trajectory(example_dataset, method())
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

  # Load required packages and namespaces
  if (!is.null(run$package_loaded) && !any(is.na(run$package_loaded)) && length(run$package_loaded)) {
    for (pack in run$package_loaded) {
      suppressMessages(do.call(require, list(pack)))
    }
  }

  if (!is.null(run$package_required) && !any(is.na(run$package_required)) && length(run$package_required)) {
    for (pack in run$package_required) {
      suppressMessages(do.call(requireNamespace, list(pack)))
    }
  }
}

.method_execution_execute_function <- function(method, inputs, priors, parameters, verbose, seed, preproc_meta) {
  # combine inputs and parameters
  args <- c(
    inputs,
    lst(
      priors,
      parameters,
      verbose,
      seed
    )
  )

  # only give args that are requested by the function
  args <- args[intersect(names(args), names(formals(method$run$run_fun)))]

  trajectory <- do.call(method$run$run_fun, args)

  trajectory
}


.method_execution_postproc_function <- function(preproc_meta) {

}


#' Generate the parameter documentation of a method, use with `@eval`
#'
#' @param definition The definition which contain the parameters
#'
#' @return A character vector containing the roxygen tags
#'
#' @export
generate_parameter_documentation <- function(definition) {
  parameter_ids <-
    names(definition$parameters$parameters)

  # generate documentation per parameter separately
  map_chr(
    parameter_ids,
    function(parameter_id) {
      parameter <- definition$parameters$parameters[[parameter_id]]
      paste0("@param ", parameter$id, " ", dynparam::get_description(parameter, sep = ". "), ".")
    }
  )
}
