#' Create a TI method from a local method definition file
#'
#' The local method definition file describes a method that is runnable on the local system.
#'
#' @param script Location of the script that will be executed. Has to contain a #!
#' @inheritParams .method_process_definition
#'
#' @importFrom yaml read_yaml
#'
#' @export
create_ti_method_definition <- function(
  definition,
  script,
  return_function = TRUE
) {
  definition <- .method_load_definition(definition)

  definition$run <- list(
    backend = "script",
    script = script
  )

  .method_process_definition(definition = definition, return_function = return_function)
}
