#' Create a TI method wrapper
#'
#' @param id A short name for the method, only lowercase characters allowed
#' @param name The name of the TI method
#' @param package_loaded The packages that need to be loaded before executing the method
#' @param package_required The packages that need to be installed before executing the method
#' @param parameters A list of parameters, which can be parsed using [parse_parameter_definition()]
#' @param par_set A bunch of parameters created by [ParamHelpers::makeParamSet()]
#' @param run_fun A function to run the TI, needs to have 'counts' as its first param.
#' @param plot_fun A function to plot the results of a TI, needs to have 'prediction' as its first param.
#' @param type The type of TI metod
#' @param ... Other information about the wrapper, eg. apt_dependencies
#' @param remotes_package Package from which the remote locations of dependencies have to be extracted, eg. `dynmethods`
#'
#' @export
#'
#' @include ti_parse_parameter_definition.R
create_ti_method <- function(
  id,
  name,
  parameters = NULL,
  par_set = NULL,
  run_fun,
  plot_fun = NULL,
  package_loaded = c(),
  package_required = c(),
  type = c("algorithm", "algorithm_test", "control", "control_test"),
  ...,
  remotes_package = ifelse("dynmethods" %in% rownames(installed.packages()), "dynmethods", "dynwrap")
) {
  if (is.null(plot_fun)) {
    plot_fun <- function(prediction) ggplot()
  }

  # process parameters
  if (is.null(parameters) && is.null(par_set)) {
    parameters <- list(dummy = list(type = "discrete", default = "dummy"))
  }

  if (is.null(par_set)) {
    par_set <- parse_parameter_definition(parameters)
  }

  default_params <- par_set %>%
    ParamHelpers::generateDesignOfDefaults(trafo = TRUE) %>%
    ParamHelpers::dfRowToList(par_set, 1)

  # process type
  type <- match.arg(type)

  # create description
  desc <- lst(
    id,
    name,
    package_loaded,
    package_required,
    par_set,
    parameters,
    type,
    ...
  ) %>% add_class("dynwrap::ti_method")

  if (is.null(desc$method_id)) {
    desc$method_id <- desc$id
  }

  if (is.character(run_fun)) {
    desc$run_fun_name <- run_fun
  }

  # construct ti function constructor
  ti_fun_constructor_with_params <- function(...) {
    # check and install dependencies of method
    install_packages(c(package_loaded, package_required), package = remotes_package, prompt = TRUE)

    # create run and plot functions
    run_fun <- get_function(run_fun)
    plot_fun <- get_function(plot_fun)

    # get the parameters from this function
    run_fun_defaults <- as.list(environment())[formalArgs(ti_fun_constructor_with_params)]

    # override default parameters in the run_fun
    formals(run_fun)[names(run_fun_defaults)] <- run_fun_defaults

    # supply run_fun to the description
    desc$run_fun <- run_fun
    desc$plot_fun <- plot_fun

    # add inputs tibble
    input_ids <- names(formals(run_fun))
    input_ids_required <- names(as.list(formals(run_fun)) %>% map_chr(class) %>% keep(~. == "name"))

    data(priors, package = "dynwrap", envir = environment())
    desc$inputs <- tibble(
      input_id = input_ids,
      required = input_id %in% input_ids_required
    ) %>% mutate(
      type = case_when(
        input_id %in% c("counts", "expression") ~ "expression",
        input_id %in% priors$prior_id ~ "prior_information",
        TRUE ~ "parameter"
      )
    )

    # return the description
    desc
  }

  formals(ti_fun_constructor_with_params) <- default_params

  ti_fun_constructor_with_params
}

#' Tests whether an object is a TI method description
#'
#' @param object The object to be tested
#'
#' @export
is_ti_method <- function(object) {
  ("dynwrap::ti_method" %in% class(object)) || ("dynmethod::ti_method" %in% class(object))
}

get_function <- function(fun) {
  if (is.character(fun)) {
    if (grepl("::", fun)) {
      parts <- strsplit(fun, "::")[[1]]
      get(parts[[2]], envir = asNamespace(parts[[1]]))
    } else {
      get(fun)
    }
  } else if (is.function(fun)) {
    fun
  } else {
    stop(sQuote("fun"), " has an incompatible format.")
  }
}

#' Get the default parameters of a method
#'
#' @param method A TI method description
#'
#' @export
#'
#' @importFrom testthat expect_true
get_default_parameters <- function(method) {
  testthat::expect_true(is_ti_method(method))

  ParamHelpers::dfRowToList(
    ParamHelpers::generateDesignOfDefaults(method$par_set, trafo = TRUE),
    method$par_set,
    i = 1
  )
}

