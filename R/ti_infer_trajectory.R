#' Infer trajectories
#'
#' @param dataset One or more datasets, as created using dynwrap
#' @param method One or more methods. Must be one of:
#' \itemize{
#'   \item{an object or list of ti_... objects (eg. [ti_comp1()])}
#'   \item{a character vector containing the names of methods to execute (e.g. `"scorpius"`), or}
#'   \item{a dynguidelines data frame.}
#' }
#' @param parameters A set of parameters to be used during trajectory inference.
#'   A parameter set must be a named list of parameters.
#'   If multiple methods were provided in the `method` parameter,
#'    `parameters` must be an unnamed list of the same length.
#' @param give_priors All the priors a method is allowed to receive.
#'   Must be a subset of all available priors (\code{\link[dynwrap:priors]{priors}}).
#' @param mc_cores The number of cores to use, allowing to parallellise the different datasets
#' @param verbose Whether or not to print information output
#' @param capture_output Whether to capture the stdout and stderr produced by a method
#'
#' @importFrom utils capture.output adist installed.packages
#' @importFrom readr read_file
#' @importFrom stringr str_length
#' @importFrom parallel mclapply
#' @importFrom testthat expect_true
#'
#' @export
infer_trajectories <- function(
  dataset,
  method,
  parameters = NULL,
  give_priors = NULL,
  mc_cores = 1,
  verbose = FALSE,
  capture_output = FALSE
) {
  # process method ----------------------
  if (is.character(method)) {
    # names of method

    # get a list of all methods
    all_desc <- get_ti_methods()

    # do some fuzzy matching
    method <- all_desc %>% slice(
      map_int(
        method,
        function(x) {
          distances <- utils::adist(
            x,
            all_desc$id,
            costs = list(
              insertions = 0.1,
              deletions = 0.5,
              substitutions = 1
            )
          )

          id <- as.integer(which.min(distances))
          if(min(distances) > 0) {
            message(stringr::str_glue("Fuzzy matching {x} -> {all_desc$id[[id]]}"))
          }

          id
        }
      )
    )

    method <- list_as_tibble(map(method$method_func, ~.()))
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

  parfun <-
    if (is.integer(mc_cores) || is.numeric(mc_cores)) {
      function(X, FUN) {
        parallel::mclapply(
          X = X,
          mc.cores = mc_cores,
          FUN = FUN
        )
      }
    } else if ("qsub::qsub_config" %in% class(mc_cores)) {
      requireNamespace("qsub")
      function(X, FUN) {
        qsub::qsub_lapply(
          X = X,
          qsub_config = mc_cores,
          qsub_packages = c("dynmethods", "dynwrap", "dynutils"),
          FUN = FUN
        )
      }
    } else {
      stop("Invalid ", sQuote("mc_cores"), " argument. Must be an integer or a qsub config.")
    }

  output <- parfun(
    X = seq_len(nrow(design)),
    FUN = function(ri) {
      tari <- dataset[[design$dataset_ix[[ri]]]]
      meri <- method[[design$method_ix[[ri]]]]
      pari <- parameters[[design$method_ix[[ri]]]]

      execute_method_on_dataset(
        dataset = tari,
        method = meri,
        parameters = pari,
        give_priors = give_priors,
        verbose = verbose,
        capture_output = capture_output
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
infer_trajectory <- function(
  dataset,
  method,
  parameters = list(),
  give_priors = NULL,
  mc_cores = 1,
  verbose = FALSE,
  ...
) {
  parameters <- c(parameters, list(...))

  design <- infer_trajectories(
    dataset = dataset,
    method = method,
    parameters = list(parameters),
    give_priors = give_priors,
    mc_cores = mc_cores,
    verbose = verbose,
    capture_output = FALSE
  )

  if(is.null(design$model[[1]])) {
    stop("Error during trajectory inference \n", design$summary[[1]]$error[[1]]$message, call. = FALSE)
  } else {
    first(design$model)
  }
}


extract_args_from_dataset <- function(
  dataset,
  inputs,
  give_priors = NULL
) {
  data("priors", package = "dynwrap", envir = environment()) # TODO: move to sysdata, avoiding loading of priors

  if(any(!give_priors %in% priors$prior_id)) {
    stop("Invalid priors requested: ", give_priors)
  }

  args_dataset <- inputs %>%
    filter(required, type == "expression") %>%
    pull(input_id) %>%
    map(get_expression, model = dataset)

  # extract prior information
  priors <- dataset$prior_information
  priors$dataset <- dataset

  # required, check if the prior infirm
  required_prior_ids <- inputs %>%
    filter(required, type == "prior_information") %>%
    pull(input_id)

  if (!all(required_prior_ids %in% names(priors))) {
    # construct informative error message for missing priors
    missing_priors <- setdiff(required_prior_ids, names(priors))
    missing_priors_text <- glue::glue_collapse(crayon::bold(missing_priors), sep = ", ", last = " and ")

    add_prior_information_params_text <- glue::glue("{missing_priors} = <prior>") %>% glue::glue_collapse(", ")
    add_prior_information_text <- crayon::italic(glue::glue("add_prior_information(dataset, {add_prior_information_params_text})"))

    stop(
      glue::glue(
        "Prior information {missing_priors_text} is missing from dataset {dataset$id} but is required by the method. \n",
        "   -> If known, you can add this prior information using {add_prior_information_text}. \n",
        "   -> Otherwise, this method cannot be used.",
        .trim = FALSE
      )
    )
  }

  args_required_priors <- priors[required_prior_ids]

  # optional
  optional_prior_ids <- inputs %>%
    filter(!required, type == "prior_information", input_id %in% give_priors) %>%
    pull(input_id)

  if (!all(optional_prior_ids %in% names(priors))) {
    warning(
      "Prior information ",
      paste(setdiff(optional_prior_ids, names(priors)), collapse = ";"),
      " is optional, but missing from dataset ",
      dataset$id,
      ". Will not give this prior to method.",
      "\n"
    )
  }

  args_optional_priors <- priors[intersect(optional_prior_ids, names(priors))]

  # output
  c(
    args_dataset,
    args_required_priors,
    args_optional_priors
  )
}


#' Run a method on a dataset with a set of parameters
#'
#' @inheritParams infer_trajectory
#' @param dataset The dataset
#' @export
execute_method_on_dataset <- function(
  dataset,
  method,
  parameters = list(),
  give_priors = NULL,
  mc_cores = 1,
  verbose = FALSE,
  capture_output = FALSE
) {
  # start the timer
  time0 <- as.numeric(Sys.time())

  # test whether the dataset contains expression
  testthat::expect_true(is_data_wrapper(dataset))
  testthat::expect_true(is_wrapper_with_expression(dataset))

  # extract args from dataset and combine with parameters
  inputs <- extract_args_from_dataset(dataset, method$inputs, give_priors)
  args <- c(
    inputs,
    parameters
  )

  if (verbose) {
    cat(
      "Executing '", method$id, "' on '", dataset$id, "'\n",
      "With parameters ", deparse(parameters), "\n",
      "And inputs ", paste0(names(inputs), collapse = ", "), "\n",
      sep = ""
    )
  }

  # add verbose if in inputs
  if ("verbose" %in% method$inputs$input_id) {
    args["verbose"] <- verbose
  }

  # initialise stdout/stderr files
  if (capture_output) {
    stdout_file <- tempfile()
    sink(stdout_file, type = "output")

    stderr_file <- tempfile()
    stderr_con <- file(stderr_file, open = "wt")
    sink(stderr_con, type = "message")
  }

  tryCatch({
    # create a temporary directory to set as working directory,
    # to avoid polluting the working directory if a method starts
    # producing files :angry_face:
    tmp_dir <- tempfile(pattern = method$id)
    dir.create(tmp_dir)
    old_wd <- getwd()
    setwd(tmp_dir)

    # run the method and catch the error, if necessary
    out <- execute_method_internal(method, args, setseed_detection_file, time0)

    # retrieve the model, error message, and timings
    model <- out$model
    error <- out$error
    timings_list <- out$timings_list

  }, finally = {
    # read in stdout/stderr
    if (capture_output) {
      sink(type = "output")
      stdout <- read_file(stdout_file)

      sink(type = "message")
      close(stderr_con)
      stderr <- read_file(stderr_file)
    } else {
      stdout <- ""
      stderr <- ""
    }

    # check whether the method produced output files and
    # wd to previous state
    num_files_created <- length(list.files(tmp_dir, recursive = TRUE))
    setwd(old_wd)

    # Remove temporary folder
    unlink(tmp_dir, recursive = TRUE, force = TRUE)
  })

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
    num_files_created = num_files_created,
    prior_df = list(method$inputs %>% rename(prior_id = input_id) %>% mutate(given = prior_id %in% names(args)))
  )

  lst(model, summary)
}

#' Internal method for executing a method
#'
#' If you're reading this, you're supposed to be using `infer_trajectory` instead.
#'
#' @inheritParams execute_method_on_dataset
#'
#' @param arglist The arguments to apply to the method
#' @param setseed_detection_file A file to which will be written if a method
#'   uses the set.seed function.
#' @param time0 The start of the timer.
#'
#' @export
#' @importFrom readr write_file
execute_method_internal <- function(method, arglist, setseed_detection_file, time0) {
  tryCatch({
    # Load required packages and namespaces
    if (!is.null(method$package_loaded) && !is.na(method$package_loaded)) {
      for (pack in method$package_loaded) {
        suppressMessages(do.call(require, list(pack)))
      }
    }

    if (!is.null(method$package_required) && !is.na(method$package_loaded)) {
      for (pack in method$package_required) {
        suppressMessages(do.call(requireNamespace, list(pack)))
      }
    }

    # measure second time point
    time_start <- as.numeric(Sys.time())

    # execute method and return model
    model <- do.call(method$run_fun, arglist)

    # measure third time point
    time_stop <- as.numeric(Sys.time())

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
}



#' Return all TI ti_methods
#'
#' @param method_ids The method identifiers. NULL if listing all methods
#' @param as_tibble Whether or not to return the ti_methods as a tibble
#' @param ti_packages In which packages to look for ti methods
#' @param evaluate Automatically evaluate the functions
#'
#' @importFrom utils lsf.str installed.packages
#' @importFrom stringr str_replace
#' @export
get_ti_methods <- function(
  method_ids = NULL,
  as_tibble = TRUE,
  ti_packages = ifelse("dynmethods" %in% rownames(utils::installed.packages()), "dynmethods", "dynwrap"),
  evaluate = FALSE
) {
  ti_methods <- map(ti_packages, function(package) {
    requireNamespace(package)

    function_names <- lsf.str(asNamespace(package), pattern = "^ti_")

    map(function_names, function(function_name) {
      meth_func <- get(function_name, asNamespace(package))

      if (evaluate) {
        meth_metadata <- meth_func() %>% discard(is.function)
      } else {
        meth_metadata <- list(id = function_name %>% stringr::str_replace("^ti_", ""))
      }
      meth_metadata$method_func <- meth_func
      meth_metadata
    })
  }) %>%
    unlist(recursive = FALSE) %>%
    list_as_tibble()

  if (!is.null(method_ids)) {
    testthat::expect_true(all(method_ids %in% ti_methods$id))
    ti_methods <- ti_methods %>% slice(match(method_ids, id))
  }

  if (as_tibble) {
    ti_methods
  } else {
    mapdf(ti_methods, identity)
  }
}
