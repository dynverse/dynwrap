.method_process_definition <- function(definition, return_function = TRUE) {
  # check definition
  assert_that(
    # check description fields
    definition %has_names% c("method_info", "parameters", "input", "output"),
    names(definition) %all_in% c("method_info", "wrapper_info", "container_info", "manuscript_info", "parameters", "input", "output", "run_info"),

    # check method info fields
    definition$method_info %has_names% c("id", "name"),
    names(definition$method_info) %all_in% c("id", "name", "implementation_id", "source", "platform", "code_url", "authors"),

    # check wrapper_info
    names(definition$wrapper_info) %all_in% c("wrapper_type", "topology_inference", "trajectory_types"),

    # check container info
    names(definition$container_info) %all_in% c("docker_repository", "container_url"),

    # check manuscript info
    names(definition$manuscript_info) %all_in% c("doi", "google_scholar_cluster_id", "preprint_date", "publication_date"),

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
  definition$parameters <- dynparam::as_parameter_set(definition$parameters)

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
    # create function with which you can instantiate the method with a set of parameters
    param_overrider_fun <- function(...) {
      # get the parameters from this function
      new_defaults <- as.list(environment())[formalArgs(param_overrider_fun)]

      # remove previous defaults
      for (param_name in names(new_defaults)) {
        if (param_name %in% names(definition$parameters)) {
          definition$parameters$parameters[[param_name]]$default <- new_defaults[[param_name]]
        } else {
          warning("Unknown parameter: ", param_name, ", skipping.")
        }
      }

      definition
    }

    formals(param_overrider_fun) <- get_default_parameters(definition)

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
