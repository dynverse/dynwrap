#' Infer trajectories
#'
#' @param dataset One or more datasets, as created using dynwrap.
#' @param method One or more methods. Must be one of:
#' \itemize{
#'   \item{an object or list of ti_... objects (eg. \code{dynmethods::ti_comp1()}),}
#'   \item{a character vector containing the names of methods to execute (e.g. `"scorpius"`),}
#'   \item{a character vector containing dockerhub repositories (e.g. `dynverse/paga`), or}
#'   \item{a dynguidelines data frame.}
#' }
#' @param parameters A set of parameters to be used during trajectory inference.
#'   A parameter set must be a named list of parameters.
#'   If multiple methods were provided in the `method` parameter,
#'    `parameters` must be an unnamed list of the same length.
#' @param give_priors All the priors a method is allowed to receive.
#'   Must be a subset of all available priors (\code{\link[dynwrap:priors]{priors}}).
#' @param seed A seed to be set, if the method allows for it.
#' @param map_fun A mao function to use when inferring trajectories with multiple datasets or methods.
#'   Allows to parallellise the execution in an arbitrary way.
#' @param verbose Whether or not to print information output.
#' @param return_verbose Whether to store and return messages printed by the method.
#' @param debug Used for debugging containers methods.
#'
#' @importFrom utils capture.output adist installed.packages
#' @importFrom readr read_file
#' @importFrom stringr str_length
#' @importFrom testthat expect_true
#'
#' @export
infer_trajectories <- function(
  dataset,
  method,
  parameters = NULL,
  give_priors = NULL,
  seed = sample(1:.Machine$integer.max, 1),
  verbose = FALSE,
  return_verbose = FALSE,
  debug = FALSE,
  map_fun = map
) {
  # process method ----------------------
  if (is.character(method) && grepl("/", method)) {
    method <- list_as_tibble(list(create_ti_method_container(method)()))

  } else if (is.character(method)) {
    # names of method

    # get a list of all methods
    descs <- get_ti_methods(method_ids = method)

    method <- list_as_tibble(map(descs$fun, ~.()))
  } else if (is_ti_method(method)) {
    # single method
    method <- list_as_tibble(list(method))
  } else if (is.data.frame(method)) {
    # dataframe
  } else if (is.list(method)) {
    # list of method
    method <- list_as_tibble(method)
  } else if ("dynguidelines::guidelines" %in% class(method)) {
    # guidelines object
    method <- method$methods_selected
  } else {
    stop("Invalid method argument, it is of class ", paste0(class(method), collapse = ", "))
  }

  # turn method(s) into a list of methods (again)
  method <- map(seq_len(nrow(method)), extract_row_to_list, tib = method)

  # process parameters ----------------
  # if not parameters given, make an empty param
  if (is.null(parameters) || length(parameters) == 0) {
    parameters <- map(seq_along(method), ~list())
  }
  testthat::expect_is(parameters, "list")

  # if a single set of parameters was given, make it a list
  if (length(method) == 1 && !is.null(names(parameters))) {
    parameters <- list(parameters)
  }

  # at this stage, parameters should be an unnamed list (with length = # methods) containing named lists
  is_list_of_paramsets <- is.null(names(parameters)) && all(map_lgl(parameters, function(x) length(x) == 0 || !is.null(names(x))))

  if (!is_list_of_paramsets) {
    stop(
      sQuote("parameters"), " must be an unnamed list of named lists, ",
      "where the named lists correspond to the parameters of methods to be executed. ",
      "If only one method is to be executed, ", sQuote("parameters"), " can also be a single ",
      "named list of parameters."
    )
  }

  # check whether parameters is of the correct length
  testthat::expect_equal(length(method), length(parameters))

  # process dataset ----------------------
  if (dynwrap::is_data_wrapper(dataset)) {
    # allow single dataset
    dataset <- list_as_tibble(list(dataset))
  } else if (is.data.frame(dataset)) {
    # dataframe of datasets
  } else if (is.list(dataset)) {
    # list of datasets
    dataset <- list_as_tibble(dataset)
  } else {
    stop("Invalid dataset argument, it is of class ", paste0(class(dataset), collapse = ", "))
  }
  dataset <- map(seq_len(nrow(dataset)), extract_row_to_list, tib = dataset)

  # Run methods on each datasets ---------
  # construct overall design
  design <- crossing(
    dataset_ix = seq_along(dataset),
    method_ix = seq_along(method)
  )

  output <- map_fun(
    seq_len(nrow(design)),
    function(ri) {
      .method_execute(
        dataset = dataset[[design$dataset_ix[[ri]]]],
        method = method[[design$method_ix[[ri]]]],
        parameters = parameters[[design$method_ix[[ri]]]],
        give_priors = give_priors,
        seed = seed,
        verbose = verbose,
        return_verbose = return_verbose,
        debug = debug
      )
    }
  )

  tibble(
    dataset_ix = design$dataset_ix,
    method_ix = design$method_ix,
    dataset_id = map_chr(dataset, "id")[design$dataset_ix],
    method_id = map_chr(method, function(m) m$method$id)[design$method_ix],
    method_name = map_chr(method, function(m) m$method$name)[design$method_ix],
    model = map(output, "model"),
    summary = map(output, "summary")
  )
}

#' @rdname infer_trajectories
#' @param ... Any additional parameters given to the method, will be concatednated to the parameters argument
#' @export
infer_trajectory <- dynutils::inherit_default_params(
  list(infer_trajectories),
  function(
    dataset,
    method,
    parameters,
    give_priors,
    seed,
    verbose,
    return_verbose,
    debug,
    ...
  ) {
    parameters <- c(parameters, list(...))

    design <- infer_trajectories(
      dataset = dataset,
      method = method,
      parameters = list(parameters),
      give_priors = give_priors,
      seed = seed,
      verbose = verbose,
      return_verbose = FALSE,
      debug = debug
    )

    if (is.null(design$model[[1]])) {
      error <- design$summary[[1]]$error[[1]]
      stop("Error during trajectory inference \n", crayon::bold(error), call. = FALSE)
    } else {
      first(design$model)
    }
  })
