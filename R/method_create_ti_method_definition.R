#' Create a TI method from a local method definition file
#'
#' The local method definition file describes a method that is runnable on the local system.
#'
#' @param filename The filename of the method definition.
#' @param definition The definition if already available as a list. Exactly one of `filename` and `definition` must be NULL.
#' @inheritParams .method_process_definition
#'
#' @importFrom yaml read_yaml
#'
#' @export
create_ti_method_definition <- function(
  filename = NULL,
  definition = NULL,
  return_function = TRUE
) {
  # either filename or text must be defined
  assert_that(is.null(filename) != is.null(definition), msg = "Either 'filename' or 'definition' must be defined")

  # read definition from file if needed
  if (is.null(definition)) {
    if (!file.exists(filename)) {
      stop("Definition could not be found at location: ", filename)
    }
    definition <- yaml::read_yaml(filename)
  }

  ######################################################
  ####                 TEST BACKEND                 ####
  ######################################################

  # todo
  # find R / python / etc executable
  # test packages?

  # save container info

  definition$run <- list(
    backend = "script"
  )

  .method_process_definition(definition = definition, return_function = return_function)
}
