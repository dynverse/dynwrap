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
  ####               CHECK DEFINITION               ####
  ######################################################

  # todo
  # find R / python / etc executable
  # test packages?

  # check container-specific ti method parameters
  assert_that(
    definition %has_names% c("input", "output", "wrapper"),

    # check wrapper format
    definition$wrapper %has_names% c("command"),
    is.character(definition$wrapper$command),

    # check input format
    definition$input %has_names% c("format"),
    length(definition$input$format) == 1,
    definition$input$format %all_in% c("hdf5", "text", "rds"),

    # check output format
    definition$output %has_names% c("format"),
    length(definition$output$format) == 1,
    definition$output$format %all_in% c("hdf5", "text", "rds", "dynwrap")
  )

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
