#' Create a definition
#'
#' A definition contains meta information on a TI method and various aspects thereof.
#' For brevity, the example only contains a minimum example, check the documentation
#' of the `def_*` helper functions for more extensive examples.
#'
#' @param method Meta information on the TI method (see [def_method()]).
#' @param wrapper Meta information on the wrapper itself (see [def_wrapper()]).
#' @param manuscript Meta information on the manuscript, if applicable (see [def_manuscript()]).
#' @param container Meta information on the container in which the wrapper resides, if applicable (see [def_container()]).
#' @param package Meta information on the package in which the wrapper resides, if applicable (see [def_package()]).
#' @param parameters Meta information on the parameters of the TI method (see [def_parameters()]).
#'
#' @keywords create_ti_method
#' 
#' @return A method definition
#'
#' @export
#'
#' @examples
#' library(dynparam)
#' definition(
#'   method = def_method(id = "some_method"),
#'   wrapper = def_wrapper(input_required = "expression"),
#'   parameters = parameter_set(
#'     integer_parameter(id = "k", default = 5L, distribution = uniform_distribution(3L, 20L))
#'   )
#' )
definition <- function(
  method,
  wrapper,
  manuscript = NULL,
  container = NULL,
  package = NULL,
  parameters = parameter_set()
) {
  definition <- as.list(environment()) %>%
    add_class("dynwrap::ti_method")

  inputs <- c(definition$wrapper$input_required, definition$wrapper$input_optional)
  params <- map_chr(definition$parameters$parameters, "id")

  definition$wrapper$inputs <-
    tibble(
      input_id = c(inputs, params),
      required = input_id %in% definition$wrapper$input_required,
      type = case_when(
        input_id %in% c("counts", "expression", "expression_future") ~ "expression",
        input_id %in% params ~ "parameter",
        TRUE ~ "prior_information"
      )
    )

  definition
}



#' Define meta information on the TI method.
#'
#' @param id An id by which to identify a method. Should only contain lowercase letters or underscores.
#' @param name The name of the method.
#' @param source The type of TI method. Options are :
#'
#'  * `"tool"`: a published TI method (peer-reviewed or preprint) (default),
#'  * `"adaptation"`: an adaptation of a published method,
#'  * `"offtheshelf"`: a method constructed from off-the-shelf algorithms,
#'  * `"control"`: a control TI method (so not actually a TI method).
#'
#' @param tool_id If there are multiple TI methods from the same toolkit, the name of the toolkit can be specified here.
#' @param platform The platform the TI method uses (e.g. R, Python, C++, ...).
#' @param url An URL to the codebase of the method.
#' @param license The software license the method uses (e.g. GPL-3, BSD-3, Artistic-2.0, MIT).
#' @param authors A list of authors (see example).
#' @param description Additional information on the method
#'
#' @keywords create_ti_method
#'
#' @export
#'
#' @examples
#' def_method(
#'   id = "some_method",
#'   name = "Some method <3",
#'   source = "tool",
#'   tool_id = "bobstoolkit",
#'   platform = "VBA",
#'   url = "https://github.com/bobdylan/singlecellvba",
#'   license = "GPL-3",
#'   authors = list(
#'     def_author(
#'       given = "Bob",
#'       family = "Dylan",
#'       email = "bob@dylan.com",
#'       github = "bobdylan",
#'       orcid = "0000-0003-1234-5678"
#'     )
#'   ),
#'   description = "I love trajectories!!"
#' )
def_method <- function(
  id,
  name = id,
  source = "tool",
  tool_id = NULL,
  platform = NULL,
  url = NULL,
  license = NULL,
  authors = list(),
  description = NULL
) {
  as.list(environment())
}

#' Meta information on an author
#'
#' @param given The given name
#' @param family The family name
#' @param email The email address
#' @param github The github handle
#' @param orcid The orcid id
#'
#' @keywords create_ti_method
#'
#' @examples
#' def_author(
#'   given = "Bob",
#'   family = "Dylan",
#'   email = "bob@dylan.com",
#'   github = "bobdylan",
#'   orcid = "0000-0003-1234-5678"
#' )
#'
#' @export
def_author <- function(
  given,
  family,
  email = NULL,
  github = NULL,
  orcid = NULL
) {
  as.list(environment())
}

#' Meta information on the manuscript
#'
#' @param doi A doi identifier (not an url)
#' @param google_scholar_cluster_id The google cluster id. Finding this id is a bit tricky;
#'   you need to find the manuscript on one of the author pages, and hover over the 'All X versions' button.
#'   Example: [google scholar page](https://goo.gl/Y9uLFs), [screenshot](https://i.imgur.com/03eLCaO.png).
#' @param preprint_date Date of publication of the preprint (format: YYYY-MM-DD).
#' @param publication_date Date of publication of the peer-reviewed manuscript (format: YYYY-MM-DD).
#'
#' @keywords create_ti_method
#'
#' @export
#'
#' @examples
#' def_manuscript(
#'   doi = "101010101/1101010101",
#'   google_scholar_cluster_id = "1010001010101111211",
#'   preprint_date = "1970-01-30",
#'   publication_date = "1970-01-31"
#' )
def_manuscript <- function(
  doi = NULL,
  google_scholar_cluster_id = NULL,
  preprint_date = NULL,
  publication_date = NULL
) {
  as.list(environment())
}

#' Meta information on the container in which the wrapper resides
#'
#' @param docker The handle of the docker container
#' @param url An url of where the docker codebase resides (containing definition.yml, Dockerfile, ...)
#'
#' @keywords create_ti_method
#'
#' @export
#'
#' @examples
#' def_container(
#'   docker = "bobdylan/ti_some_method",
#'   url = "https://github.com/bobdylan/ti_some_method"
#' )
def_container <- function(
  docker,
  url = NULL
) {
  as.list(environment())
}

#' Meta information on the package in which the TI function resides
#'
#' @param name The name of the package
#' @param remote The github repository handle
#' @param function_name The name of the function
#'
#' @keywords create_ti_method
#'
#' @export
#'
#' @examples
#' def_package(
#'   remote = "rcannood/SCORPIUS",
#'   name = "SCORPIUS",
#'   function_name = "ti_scorpius"
#' )
def_package <- function(
  remote,
  name,
  function_name
) {
  as.list(environment())
}


#' Meta information on the wrapper
#'
#' @param input_required The required inputs for this method. See `dynwrap::allowed_inputs()`.
#' @param input_optional Optional inputs for this method. See `dynwrap::allowed_inputs()`.
#' @param type Which type of trajectory post-processing is used. Possible values:
#'   `"trajectory"` (default), `"linear_trajectory"`, `"cyclic_trajectory"`, `"branch_trajectory"`,
#'   `"cluster_graph"`, `"dimred_projection"`, `"end_state_probabilities"`, `"cell_graph"`.
#' @param topology_inference Whether the topology is fixed (`"fixed"`), free (`"free"`),
#'   or fixed by a parameter provided to the algorithm (`"param"`).
#' @param trajectory_types The possible trajectory types this method can return. Must be a subset of
#'   `c("cyclic", "linear", "bifurcation", "convergence", "multifurcation", "tree", "graph", "acyclic_graph", "disconnected_graph")`
#'
#' @keywords create_ti_method
#'
#' @export
#'
#' @examples
#' def_wrapper(
#'   input_required = c("expression", "start_id"),
#'   input_optional = "groups_n",
#'   type = "dimred_projection",
#'   trajectory_types = c("linear", "cyclic"),
#'   topology_inference = "free"
#' )
def_wrapper <- function(
  input_required,
  input_optional = character(),
  type = "trajectory",
  topology_inference = NULL,
  trajectory_types = character()
) {
  wrapper <- as.list(environment())

  # make sure all inputs are allowed
  # disabling this to allow for custom priors!
  # assert_that(
  #   wrapper$input_required %all_in% dynwrap::allowed_inputs$input_id,
  #   wrapper$input_optional %all_in% dynwrap::allowed_inputs$input_id
  # )

  wrapper
}


#' Meta information on the parameters of the TI method
#'
#' Parameters can be defined using [dynparam::dynparam()].
#'
#' @inheritParams dynparam::parameter_set
#'
#' @keywords create_ti_method
#'
#' @export
#'
#' @examples
#' library(dynparam)
#' def_parameters(
#'   character_parameter(id = "method", default = "one", values = c("one", "two", "three")),
#'   integer_parameter(
#'     id = "ndim",
#'     default = 3L,
#'     distribution = uniform_distribution(lower = 2L, upper = 20L)
#'   ),
#'   numeric_parameter(
#'     id = "beta",
#'     default = 0.005,
#'     distribution = expuniform_distribution(lower = 1e-10, upper = 1)
#'   )
#' )
def_parameters <- dynparam::parameter_set

#' Get the default parameters of a method
#'
#' @param definition A TI method description
#'
#' @keywords create_ti_method
#'
#' @export
get_default_parameters <- function(definition) {
  assert_that(is_ti_method(definition))

  map(definition$parameters$parameters, ~ .$default)
}


#' @rdname definition
#'
#' @export
is_ti_method <- function(method) {
  ("dynwrap::ti_method" %in% class(method)) || ("dynmethod::ti_method" %in% class(method))
}



#' Convert a definition loaded in from a yaml
#'
#' @param definition_raw The raw definition loaded from the yaml
#'
#' @keywords create_ti_method
#'
#' @export
convert_definition <- function(definition_raw) {
  definition(
    method = purrr::invoke(def_method, definition_raw$method %||% list()),
    wrapper = purrr::invoke(def_wrapper, definition_raw$wrapper %||% list()),
    container = purrr::invoke(def_container, definition_raw$container %||% list()),
    package = purrr::invoke(def_package, definition_raw$package %||% list()),
    manuscript = purrr::invoke(def_manuscript, definition_raw$manuscript %||% list()),
    parameters = dynparam::as_parameter_set(definition_raw$parameters %||% list())
  )
}



# definition is character -> load in definition from file and process as definition
# else, assume this is already a correct definition and just return
.method_load_definition <- function(definition) {
  if (is.character(definition)) {
    assert_that(is.character(definition), length(definition) == 1)
    convert_definition(yaml::read_yaml(definition))
  } else {
    definition
  }
}

#' Method process definition
#' @param definition A definition, see [definition()]
#' @param return_function Whether to return a function that allows you to override the default parameters, or just return the method meta data as is.
#'
#' @importFrom methods formalArgs
#'
#' @keywords create_ti_method
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
