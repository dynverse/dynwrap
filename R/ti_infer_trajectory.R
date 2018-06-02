#' Infer trajectories
#'
#' @param task One or more datasets, as created using dynwrap
#' @param method One or more methods. Must be one of:
#' \itemize{
#'   \item{an object or list of ti_... objects (e.g. [ti_comp1()]),}
#'   \item{a character vector containing the names of methods to execute (e.g. `"scorpius"`), or}
#'   \item{a dynguidelines data frame.}
#' }
#' @param parameters A set of parameters to be used during trajectory inference.
#'   A parameter set must be a named list of parameters.
#'   If multiple methods were provided in the `method` parameter,
#'    `parameters` must be an unnamed list of the same length.
#' @param give_priors All the priors a method is allowed to receive. Must be a subset of: `"start_milestones"`,
#'  `"start_cells"`, `"end_milestones"`, `"end_cells"`, `"grouping_assignment"` and `"grouping_network"`
#' @param mc_cores The number of cores to use, allowing to parallellise the different tasks
#' @param verbose Whether or not to print information output
#'
#' @importFrom utils capture.output
#' @importFrom readr read_file
#' @importFrom stringr str_length
#' @importFrom parallel mclapply
#' @importFrom testthat expect_true
#' @export
infer_trajectories <- function(
  task,
  method,
  parameters = NULL,
  give_priors = NULL,
  mc_cores = 1,
  verbose = FALSE
) {
  if (verbose) {
    if (nrow(task) == 1) {
      task_str <- task$id
    } else {
      task_str <- paste0(nrow(task), " tasks")
    }
    cat("Executing ", method$name, " on ", task_str, "\n", sep = "")
  }

  # process method ----------------------
  if (is.character(method)) {
    # names of method

    # get a list of all methods
    packages <- if("dynmethods" %in% rownames(installed.packages())) {
      c("dynmethods", "dynwrap")
    } else {
      "dynwrap"
    }
    all_desc <- get_ti_methods(packages = packages)

    # do some fuzzy matching, try both short name and real name
    method <- all_desc %>% slice(
      map_int(
        method,
        function(x) {
          distances <- adist(x, c(all_desc$name, all_desc$short_name))
          id <- as.integer(((which.min(distances)-1) %% nrow(all_desc)) + 1)
          if(min(distances) > 0) {
            message(stringr::str_glue("Fuzzy matching {x} -> {all_desc$name[[id]]} / {all_desc$short_name[[id]]}"))
          }

          id
        }
      )
    )

    method
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
  if (is.null(parameters) || length(parameters) == 0) {
    parameters <- lapply(seq_along(method), function(i) list())
  }
  testthat::expect_is(parameters, "list")

  is_paramset <- nrow(method) == 1 && !is.null(names(parameters))
  is_list_of_paramsets <- is.null(names(parameters)) && all(map_lgl(parameters, function(x) length(x) == 0 || !is.null(names(x))))

  if (!is_paramset && !is_list_of_paramsets) {
    stop(
      sQuote("parameters"), " must be an unnamed list of named lists, ",
      "where the named lists correspond to the parameters of methods to be executed. ",
      "If only one method is to be executed, ", sQuote("parameters"), " can also be a single ",
      "named list of parameters."
    )
  }

  if (is_paramset) {
    parameters <- list(parameters)
  }

  # check whether parameters is of the correct length
  testthat::expect_equal(length(method), length(parameters))

  # process task ----------------------
  if(dynwrap::is_data_wrapper(task)) {
    # allow single task
    task <- list_as_tibble(list(task))
  } else if (is.data.frame(task)) {
    # dataframe of tasks
  } else if (is.list(task)) {
    # list of tasks
    task <- list_as_tibble(task)
  } else {
    stop("Invalid task argument, it is of class ", paste0(class(task), collapse = ", "))
  }
  task <- map(seq_len(nrow(task)), extract_row_to_list, tib = task)

  # Run methods on each tasks ---------
  # construct overall design
  design <- crossing(
    taski = seq_along(task),
    methodi = seq_along(method)
  )

  output <- parallel::mclapply(
    X = seq_len(nrow(design)),
    mc.cores = mc_cores,
    FUN = function(ri) {
      tari <- task[[design$taski[[ri]]]]
      meri <- method[[design$methodi[[ri]]]]
      pari <- parameters[[design$methodi[[ri]]]]

      execute_method_on_task(
        task = tari,
        method = meri,
        parameters = pari,
        give_priors = give_priors,
        verbose = verbose
      )
    }
  )

  tibble(
    task_ix = design$taski,
    method_ix = design$methodi,
    task_id = map_chr(task, "id")[design$taski],
    method_name = map_chr(method, "name")[design$methodi],
    model = map(output, "model"),
    summary = map(output, "summary")
  )
}


#' @rdname infer_trajectories
#' @param ... Any additional parameters given to the method, will be concatednated to the parameters argument
#' @export
infer_trajectory <- function(
  task,
  method,
  parameters = list(),
  give_priors = NULL,
  mc_cores = 1,
  verbose = FALSE,
  ...
) {
  parameters <- c(parameters, list(...))

  design <- infer_trajectories(
    task = task,
    method = method,
    parameters = list(parameters),
    give_priors = give_priors,
    mc_cores = mc_cores,
    verbose = verbose
  )

  if(is.null(design$model[[1]])) {
    stop("Error during trajectory inference \n", design$summary[[1]]$error)
  } else {
    first(design$model)
  }
}


extract_args_from_task <- function(
  task,
  inputs,
  give_priors = NULL
) {
  if(!any(give_priors %in% priors$prior_id2)) {
    stop("Invalid priors requested: ", give_priors)
  }

  args_task <- task[inputs %>% filter(required, type == "expression") %>% pull(input_id)]

  # extract prior information
  priors <- task$prior_information
  priors$task <- task

  # required, check if the prior infirm
  required_prior_ids <- inputs %>%
    filter(required, type == "prior_information") %>%
    pull(input_id)

  if (!all(required_prior_ids %in% names(priors))) {
    stop(
      "Prior information ",
      paste(setdiff(required_prior_ids, names(priors)), collapse = ";"),
      " is required, but missing from task ",
      task$id)
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
      " is optional, but missing from task ",
      task$id,
      ". Will not give this prior to method.",
      "\n")
  }

  args_optional_priors <- priors[intersect(optional_prior_ids, names(priors))]

  # output
  c(
    args_task,
    args_required_priors,
    args_optional_priors
  )
}


#' Run a method on a task with a set of parameters
#'
#' @inheritParams infer_trajectory
#' @param task The task
#' @export
execute_method_on_task <- function(
  task,
  method,
  parameters = list(),
  give_priors = NULL,
  mc_cores = 1,
  verbose = FALSE
) {
  # start the timer
  time0 <- Sys.time()

  # test whether the task contains expression
  testthat::expect_true(is_data_wrapper(task))
  testthat::expect_true(is_wrapper_with_expression(task))

  # extract args from task and combine with parameters
  args <- c(
    extract_args_from_task(task, method$inputs, give_priors),
    parameters
  )

  # create a temporary directory to set as working directory,
  # to avoid polluting the working directory if a method starts
  # producing files :angry_face:
  tmp_dir <- tempfile(pattern = method$short_name)
  dir.create(tmp_dir)
  old_wd <- getwd()
  setwd(tmp_dir)

  # disable seed setting
  # a method shouldn't set seeds during regular execution,
  # it should be left up to the user instead
  orig_setseed <- base::set.seed
  setseed_detection_file <- tempfile(pattern = "seedsetcheck")

  # run the method and catch the error, if necessary
  out <-
    tryCatch({
      # run method
      model <- execute_method_internal(method, args, setseed_detection_file)

      # add task id and method names to the model
      model$task_id <- task$id
      model$method_name <- method$name
      model$method_short_name <- method$short_name

      c(model, list(error = NULL))
    }, error = function(e) {
      time_new <- Sys.time()
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

  # check whether the method produced output files and
  # wd to previous state
  num_files_created <- length(list.files(tmp_dir, recursive = TRUE))
  setwd(old_wd)

  # Remove temporary folder
  unlink(tmp_dir, recursive = TRUE, force = TRUE)

  # read how many seeds were set and
  # restore environment to previous state
  num_setseed_calls <-
    if (file.exists(setseed_detection_file)) {
      stringr::str_length(readr::read_file(setseed_detection_file))
    } else {
      0
    }
  if (file.exists(setseed_detection_file)) {
    file.remove(setseed_detection_file)
  }
  dynutils::override_setseed(orig_setseed)

  # stop the timer
  time3 <- Sys.time()

  # create a summary tibble
  summary <- tibble(
    method_name = method$name,
    method_short_name = method$short_name,
    task_id = task$id,
    time_sessionsetup = as.numeric(difftime(timings_list$method_start, time0, units = "sec")),
    time_preprocessing = as.numeric(difftime(timings_list$method_afterpreproc, timings_list$method_start, units = "sec")),
    time_method = as.numeric(difftime(timings_list$method_aftermethod, timings_list$method_afterpreproc, units = "sec")),
    time_postprocessing = as.numeric(difftime(timings_list$method_afterpostproc, timings_list$method_aftermethod, units = "sec")),
    time_wrapping = as.numeric(difftime(timings_list$method_stop, timings_list$method_afterpostproc, units = "sec")),
    time_sessioncleanup = as.numeric(difftime(time3, timings_list$method_stop, units = "sec")),
    error = list(error),
    num_files_created = num_files_created,
    num_setseed_calls = num_setseed_calls,
    prior_df = list(method$inputs %>% rename(prior_id = input_id) %>% mutate(given = prior_id %in% names(args)))
  )

  lst(model, summary)
}

#' Internal method for executing a method
#'
#' If you're reading this, you're supposed to be using `infer_trajectory` instead.
#'
#' @inheritParams execute_method_on_task
#'
#' @param arglist The arguments to apply to the method
#' @param setseed_detection_file A file to which will be written if a method
#'   uses the set.seed function.
#'
#' @export
#' @importFrom readr write_file
execute_method_internal <- function(method, arglist, setseed_detection_file) {
  # disable seed setting
  # a method shouldn't set seeds during regular execution,
  # it should be left up to the user instead
  new_setseed <- function(i) {
    readr::write_file("1", setseed_detection_file, append = TRUE)
  }
  dynutils::override_setseed(new_setseed)

  # Load required packages and namespaces
  for (pack in method$package_loaded) {
    suppressMessages(do.call(require, list(pack)))
  }
  for (pack in method$package_required) {
    suppressMessages(do.call(requireNamespace, list(pack)))
  }

  # measure second time point
  time_start <- Sys.time()

  # execute method and return model
  model <- do.call(method$run_fun, arglist)

  # measure third time point
  time_stop <- Sys.time()

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
    list(method_start = time_start),
    model$timings,
    list(method_stop = time_stop)
  )

  model$timings <- NULL
  class(model) <- class(model) %>% discard(~. %in% c("dynutils::with_timings", "dynwrap::with_timings"))

  # return output
  lst(timings_list, model)
}



#' Return all TI ti_methods
#'
#' @param as_tibble Whether or not to return the ti_methods as a tibble
#' @param packages In which packages to look for ti methods
#'
#' @importFrom utils lsf.str
#' @export
get_ti_methods <- function(as_tibble = TRUE, packages = c("dynwrap")) {
  ti_methods <- map(packages, function(package) {
    requireNamespace(package)

    lsf.str(asNamespace(package), pattern = "^ti_") %>%
      map(~ do.call(., args = list(), envir = asNamespace(package)))
  }) %>% unlist(recursive=FALSE)

  if (as_tibble) {
    list_as_tibble(ti_methods)
  } else {
    ti_methods %>% setNames(ti_methods %>% map_chr(~.$short_name))
  }
}
