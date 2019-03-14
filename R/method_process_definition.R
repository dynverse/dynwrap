#' Create a definition
#' @param method See [def_method()]
#' @param wrapper See [def_wrapper()]
#' @param manuscript See [def_manuscript()]
#' @param container See [def_container()]
#' @param parameters See [def_parameters()]
#'
#' @export
definition <- function(
  method,
  wrapper,
  manuscript = NULL,
  container = NULL,
  parameters = parameter_set()
) {
  definition <- as.list(environment()) %>%
    add_class("dynwrap::ti_method")

  # parse the inputs
  utils::data("priors", package = "dynwrap", envir = environment()) # TODO: move to sysdata, avoiding loading of priors
  definition$wrapper$inputs <-
    tibble(
      input_id = c(definition$wrapper$input_required, definition$wrapper$input_optional, map_chr(definition$parameters$parameters, "id")),
      required = input_id %in% definition$wrapper$input_required,
      type = case_when(
        input_id %in% c("counts", "expression") ~ "expression",
        input_id %in% priors$prior_id ~ "prior_information",
        TRUE ~ "parameter"
      )
    )

  definition
}



#' Define the method
#'
#' @param id id
#' @param name name
#' @param source source
#' @param platform platform
#' @param url url
#' @param license license
#' @param authors authors
#'
#' @export
def_method <- function(
  id,
  name = id,
  source = "tool",
  platform = "R",
  url = NULL,
  license = NULL,
  authors = list()
) {
  as.list(environment())
}

#' Define the manuscript
#'
#' @param doi doi
#' @param google_scholar_cluster_id google_scholar_cluster_id
#' @param preprint_date preprint_data
#' @param publication_date publication_date
#'
#' @export
def_manuscript <- function(
  doi = NULL,
  google_scholar_cluster_id = NULL,
  preprint_date = NULL,
  publication_date = NULL
) {
  as.list(environment())
}

#' Define the container
#'
#' @param docker docker
#' @param url url
#'
#' @export
def_container <- function(
  docker,
  url = NULL
) {
  as.list(environment())
}


#' Define the wrapper
#'
#' @param input_required The required inputs for this method. See `dynwrap::allowed_inputs()`.
#' @param input_optional Optional inputs for this method. See `dynwrap::allowed_inputs()`.
#' @param type type
#' @param topology_inference topology_inference
#' @param trajectory_types trajectory_types
#'
#' @export
def_wrapper <- function(
  input_required,
  input_optional = character(),
  type = "trajectory",
  topology_inference = NULL,
  trajectory_types = character()
) {
  wrapper <- as.list(environment())

  # make sure all inputs are allowed
  assert_that(
    wrapper$input_required %all_in% dynwrap::allowed_inputs$input_id,
    wrapper$input_optional %all_in% dynwrap::allowed_inputs$input_id
  )

  wrapper
}


#' Define the parameters
#'
#' @param ... The parameters! :-)
#'
#' @export
def_parameters <- function(
  ...
) {
  dynparam::parameter_set(...)
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


# definition is character -> load in definition from file and process as definition
# else, assume this is already a correct definition and just return
.method_load_definition <- function(definition) {
  if (is.character(definition)) {
    assert_that(length(definition) == 1)
    definition_raw <- yaml::read_yaml(filename)

    definition
  } else {
    definition
  }
}

# convert a definition loaded from a yaml
.method_convert_definition <- function(definition_raw) {
  definition(
    method = purrr::invoke(def_method, definition_raw$method %||% list()),
    parameters = purrr::invoke(def_method, definition_raw$parameters %||% list()),
    manuscript = purrr::invoke(def_manuscript, definition_raw$manuscript %||% list()),
    wrapper = purrr::invoke(def_wrapper, definition_raw$wrapper %||% list()),
    container = purrr::invoke(def_container, definition_raw$container %||% list())
  )
}


#' Method process definition
#' @param definition A definition, see [definition()]
#' @param return_function Whether to return a function that allows you to override the default parameters, or just return the method meta data as is.
.method_process_definition <- function(definition, return_function) {
  # return function if requested
  if (!return_function) {
    definition
  } else {
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
          stop("Unknown parameter: ", param_name)
        }
      }

      definition
    }

    formals(param_overrider_fun) <- defaults

    param_overrider_fun
  }
}
