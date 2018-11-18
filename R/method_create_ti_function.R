
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
