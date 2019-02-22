#' Process TI method description
#'
#' Assert correctness of common parameters and process priors and parameters
#'
#' @param definition The description of the TI method, containing meta data and information on how to execute it.
#' @param return_function Whether to return a function that allows you to override the default parameters, or just return the method meta data as is.
.method_process_definition <- function(definition, return_function = TRUE) {
  # check definition
  assert_that(
    # check description fields
    definition %has_names% c("method", "parameters", "input", "output"),
    names(definition) %all_in% c("method", "wrapper", "container", "manuscript", "parameters", "input", "output", "run"),

    # check method info fields
    definition$method %has_names% c("id", "name"),
    names(definition$method) %all_in% c("id", "name", "tool_id", "source", "platform", "url", "authors", "license"),

    # check wrapper
    names(definition$wrapper) %all_in% c("type", "topology_inference", "trajectory_types", "command", "example"),

    # check container info
    names(definition$container) %all_in% c("docker", "url"),

    # check manuscript info
    names(definition$manuscript) %all_in% c("doi", "google_scholar_cluster_id", "preprint_date", "publication_date"),

    # check inputs
    definition$input %has_names% c("required"),
    names(definition$input) %all_in% c("format", "required", "optional"),
    definition$input$required %all_in% dynwrap::allowed_inputs$input_id,
    definition$input$optional %all_in% dynwrap::allowed_inputs$input_id,

    # check outputs
    definition$output %has_names% c("outputs"),
    names(definition$output) %all_in% c("format", "outputs"),
    definition$output$outputs %all_in% dynwrap::allowed_outputs$output_id
  )

  # parse the parameters
  if (!dynparam::is_parameter_set(definition$parameters)) {
    definition$parameters <- dynparam::as_parameter_set(definition$parameters)
  }

  # parse the inputs
  utils::data("priors", package = "dynwrap", envir = environment()) # TODO: move to sysdata, avoiding loading of priors
  definition$inputs <-
    tibble(
      input_id = c(definition$input$required, definition$input$optional, definition$parameters$id),
      required = input_id %in% definition$input$required,
      type = case_when(
        input_id %in% c("counts", "expression") ~ "expression",
        input_id %in% priors$prior_id ~ "prior_information",
        TRUE ~ "parameter"
      )
    )

  # add class to definition
  definition <- definition %>%
    add_class("dynwrap::ti_method")

  if (return_function) {
    defaults <- get_default_parameters(definition)

    # create function with which you can instantiate the method with a set of parameters
    param_overrider_fun <- function(...) {
      # get the parameters from this function
      new_defaults <- as.list(environment())[formalArgs(param_overrider_fun)]

      param_names <- map_chr(definition$parameters$parameters, "id")
      # remove previous defaults
      for (param_name in names(new_defaults)) {

        if (param_name %in% param_names) {
          definition$parameters$parameters[[which(param_name == param_names)]]$default <- new_defaults[[param_name]]
        } else {
          warning("Unknown parameter: ", param_name, ", skipping.")
        }
      }

      definition
    }

    formals(param_overrider_fun) <- defaults

    param_overrider_fun
  } else {
    definition
  }
}



#' Tests whether an object is a TI method description
#'
#' @param object The object to be tested
#'
#' @export
is_ti_method <- function(object) {
  ("dynwrap::ti_method" %in% class(object)) || ("dynmethod::ti_method" %in% class(object))
}

#' Get the default parameters of a method
#'
#' @param definition A TI method description
#'
#' @export
#'
#' @importFrom testthat expect_true
get_default_parameters <- function(definition) {
  testthat::expect_true(is_ti_method(definition))

  map(definition$parameters$parameters, ~ .$default)
}
