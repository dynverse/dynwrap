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
#' @param capture_output Whether to capture the stdout and stderr produced by a method.
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
  seed = 1,
  map_fun = map,
  verbose = FALSE,
  capture_output = FALSE
) {
  # process method ----------------------
  if (is.character(method) && grepl("/", method)) {
    method <- list_as_tibble(list(create_ti_method_with_container(method)()))

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
  if(dynwrap::is_data_wrapper(dataset)) {
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
        verbose = verbose,
        capture_output = capture_output,
        seed = seed
      )
    }
  )

  tibble(
    dataset_ix = design$dataset_ix,
    method_ix = design$method_ix,
    dataset_id = map_chr(dataset, "id")[design$dataset_ix],
    method_id = map_chr(method, "id")[design$method_ix],
    method_name = map_chr(method, "name")[design$method_ix],
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
    map_fun,
    verbose,
    ...
  ) {
    parameters <- c(parameters, list(...))

    design <- infer_trajectories(
      dataset = dataset,
      method = method,
      parameters = list(parameters),
      give_priors = give_priors,
      map_fun = map_fun,
      verbose = verbose,
      capture_output = FALSE,
      seed = seed
    )

    if (is.null(design$model[[1]])) {
      error <- design$summary[[1]]$error[[1]]
      cat("Error traceback:\n")
      traceback(error)
      stop("Error during trajectory inference \n", error$message, call. = FALSE)
    } else {
      first(design$model)
    }
  })


.method_execute <- function(
  dataset,
  method,
  parameters,
  give_priors,
  verbose,
  capture_output,
  seed
) {
  # start the timer
  time0 <- as.numeric(Sys.time())

  # test whether the dataset contains expression
  testthat::expect_true(is_data_wrapper(dataset))
  testthat::expect_true(is_wrapper_with_expression(dataset))

  # check method
  testthat::expect_true(is_ti_method(method))

  # extract args from dataset
  inputs <- .method_extract_args(dataset, method$inputs, give_priors)

  # extract parameters from method
  params <- get_default_parameters(method)
  params[names(parameters)] <- parameters
  parameters <- params
  rm(params)

  # combine inputs and parameters
  args <- c(
    inputs,
    parameters
  )

  # add extra params
  if ("verbose" %in% method$inputs$input_id) args["verbose"] <- verbose
  if ("seed" %in% method$inputs$input_id) args["seed"] <- verbose

  # remove params that are not supposed to be here
  remove_args <- setdiff(c(names(args)), method$inputs$input_id)
  if (length(remove_args) > 0) {
    warning("Parameters [", paste(remove_args, ", "), "] not recognised by method; removing them from the arglist.")
    sel_args <- setdiff(names(args), remove_args)
    args <- args[remove_args]
  }

  # print helpful message
  if (verbose) {
    cat(
      "Executing '", method$id, "' on '", dataset$id, "'\n",
      "With parameters: ", deparse(parameters), "\n",
      "And inputs: ", paste0(names(inputs), collapse = ", "), "\n",
      sep = ""
    )
  }

  run_info <- method$run_info

  # run preproc
  preproc_meta <-
    if (run_info$backend == "function") {
      .method_execute_preproc_function(run_info)
    } else {
      .method_execute_preproc_container(run_info)
    }

  # run the method and catch the error, if necessary
  out <- tryCatch({
    # measure second time point
    time_start <- as.numeric(Sys.time())

    # execute method and return model
    if (run_info$backend == "function") {
      .method_execute_run_function(...)
    } else {
      .method_execute_run_container(...)
    }

    # measure third time point
    time_stop <- as.numeric(Sys.time())

    # run postproc
    if (run_info$backend == "function") {
      .method_execute_postproc_function(...)
    } else {
      .method_execute_postproc_container(...)
    }

    # add missing timings
    if (is.null(model$timings)) {
      model$timings <- list(
        method_afterpreproc = time_start,
        method_aftermethod = time_stop,
        method_afterpostproc = time_stop
      )
    }

    # fetch timings from within method (and place them in order of execution, just to make sure)
    timings_list <- c(
      list(method_start = as.numeric(time_start)),
      model$timings,
      list(method_stop = as.numeric(time_stop))
    )

    model$timings <- NULL
    class(model) <- setdiff(class(model), "dynwrap::with_timings")

    # return output
    lst(timings_list, model, error = NULL)
  }, error = function(e) {
    time_new <- as.numeric(Sys.time())
    timings_list <- list(
      method_start = time0,
      method_afterpreproc = time0,
      method_aftermethod = time_new,
      method_afterpostproc = time_new,
      method_stop = time_new
    )
    list(model = NULL, timings_list = timings_list, error = e)
  })


  # retrieve the model, error message, and timings
  model <- out$model
  error <- out$error
  timings_list <- out$timings_list

  # stop the timer
  time3 <- as.numeric(Sys.time())

  timings_list <- map(timings_list, as.numeric)

  # add missing timings
  timings_list[
    setdiff(
      c("method_start", "method_afterpreproc", "method_aftermethod", "method_afterpostproc", "method_stop"),
      names(timings_list)
    )
    ] <- time3

  # create a summary tibble
  summary <- tibble(
    method_name = method$name,
    method_id = method$id,
    dataset_id = dataset$id,
    time_sessionsetup = timings_list$method_start - time0,
    time_preprocessing = timings_list$method_afterpreproc - timings_list$method_start,
    time_method = timings_list$method_aftermethod - timings_list$method_afterpreproc,
    time_postprocessing = timings_list$method_afterpostproc - timings_list$method_aftermethod,
    time_wrapping = timings_list$method_stop - timings_list$method_afterpostproc,
    time_sessioncleanup = time3 - timings_list$method_stop,
    error = list(error),
    stdout = stdout,
    stderr = stderr,
    prior_df = list(method$inputs %>% rename(prior_id = input_id) %>% mutate(given = prior_id %in% names(args)))
  )

  lst(model, summary)
}
