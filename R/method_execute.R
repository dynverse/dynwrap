.method_execute <- function(
  dataset,
  method,
  parameters,
  give_priors,
  seed,
  verbose,
  return_verbose,
  debug
) {
  if (debug) verbose <- TRUE

  # start the timer
  timings <- list(execution_start = Sys.time())

  assert_that(
    is_data_wrapper(dataset),
    is_wrapper_with_expression(dataset),
    is_ti_method(method)
  )

  # extract inputs from dataset
  inputs <- .method_extract_inputs(dataset, method$wrapper$inputs)

  # extract priors from dataset
  priors <- .method_extract_priors(dataset, method$wrapper$inputs, give_priors)

  # extract parameters from method
  parameters_ <- get_default_parameters(method)
  parameters_[names(parameters)] <- parameters
  parameters <- parameters_
  rm(parameters_)

  # initialise stdout/stderr files
  sink_meta <- .method_init_sinks(
    verbose = verbose,
    return_verbose = return_verbose
  )

  # create a temporary directory to set as working directory,
  # to avoid polluting the working directory if a method starts
  # producing files haphazardly
  tmp_wd <- tempfile(pattern = method$method$id)
  dir.create(tmp_wd)
  old_wd <- setwd(tmp_wd)

  # reset to old wd upon exit
  on.exit({
    setwd(old_wd)
    .method_close_sinks(sink_meta)
    unlink(tmp_wd, recursive = TRUE, force = TRUE)
  })

  tryCatch({
    # print helpful message
    if (verbose) {
      cat(
        "Executing '", method$method$id, "' on '", dataset$id, "'\n",
        "With parameters: ", deparse(parameters), "\n",
        "inputs: ", paste0(names(inputs), collapse = ", "), "\n",
        "priors : ", paste0(names(priors), collapse = ", "), "\n",
        sep = ""
      )
    }

    # run preproc
    preproc_meta <-
      if (method$run$backend == "function") {
        .method_execution_preproc_function(method = method)
      } else {
        .method_execution_preproc_container(method = method, inputs = inputs, priors = priors, parameters = parameters, verbose = verbose || return_verbose, seed = seed, debug = debug)
      }

    # initialise output variables
    trajectory <- NULL
    timings$method_beforepreproc <- Sys.time()

    error <- tryCatch({
      # execute method and return trajectory
      trajectory <-
        if (method$run$backend == "function") {
          .method_execution_execute_function(method = method, inputs = inputs, priors = priors, parameters = parameters, verbose = verbose || return_verbose, seed = seed, preproc_meta = preproc_meta)
        } else if (method$run$backend == "script") {
          .method_execution_execute_script(method = method, preproc_meta = preproc_meta)
        } else {
          .method_execution_execute_container(method = method, preproc_meta = preproc_meta)
        }

      # add trajectory timings and timings stop
      timings <- c(timings, trajectory$timings)
      timings$method_afterpostproc <- Sys.time()

      # remove timings from trajectory
      trajectory$timings <- NULL
      class(trajectory) <- setdiff(class(trajectory), "dynwrap::with_timings")

      NA_character_
    }, error = function(e) {
      as.character(e)
    })

    # run postproc
    if (method$run$backend == "function") {
      .method_execution_postproc_function(preproc_meta = preproc_meta) # empty
    } else {
      .method_execution_postproc_container(preproc_meta = preproc_meta)
    }

    # revert to old wd and retrieve stdout/stderr
    setwd(old_wd)
    stds <- .method_close_sinks(sink_meta)
    on.exit({}, add = FALSE)
    unlink(tmp_wd, recursive = TRUE, force = TRUE)

    # stop timings
    timings$execution_stop <- Sys.time()

    # if method doesn't return these timings, row with the oars we have
    if (!"method_afterpreproc" %in% names(timings)) timings$method_afterpreproc <- timings$method_beforepreproc
    if (!"method_aftermethod" %in% names(timings)) timings$method_aftermethod <- timings$method_afterpostproc

    # make sure timings are numeric
    timings <- map_dbl(timings, as.numeric)

    # calculate timing differences
    timings_diff <- diff(timings[c("execution_start", "method_beforepreproc", "method_afterpreproc", "method_aftermethod", "method_afterpostproc", "execution_stop")]) %>%
      set_names(c("time_sessionsetup", "time_preprocessing", "time_method", "time_postprocessing", "time_sessioncleanup"))

    # create a summary tibble
    summary <- tibble(
      method_name = method$method$name,
      method_id = method$method$id,
      dataset_id = dataset$id,
      stdout = stds$stdout,
      stderr = stds$stderr,
      error = error,
      prior_df = list(method$wrapper$inputs %>% rename(prior_id = input_id) %>% mutate(given = prior_id %in% names(inputs)))
    ) %>%
      bind_cols(as.data.frame(as.list(timings_diff)))
  }, error = function(e) {
    stop("Error produced in dynwrap input/output:\n", as.character(e))
  })

  lst(trajectory, summary)
}

.method_init_sinks <- function(verbose, return_verbose) {
  if (!verbose || return_verbose) {
    stdout_file <- tempfile()
    sink(stdout_file, type = "output", split = verbose, append = TRUE)

    # manual states that messages can only be sinked with an open connection
    stderr_file <- tempfile()
    stderr_con <- file(stderr_file, open = "wt")

    # can't split the message connection :(
    sink(stderr_con, type = "message", split = FALSE, append = TRUE)
  } else {
    stdout_file <- stderr_file <- stderr_con <- NULL
  }

  lst(
    stdout_file,
    stderr_file,
    stderr_con,
    verbose,
    return_verbose
  )
}
.method_close_sinks <- function(sink_meta) {
  # close sinks
  if (sink.number(type = c("output", "message")) > 0) {
    if (!sink_meta$verbose || sink_meta$return_verbose) {
      sink(type = "output")
      sink(type = "message")
      if (!is.null(sink_meta$stderr_con) && !isTRUE(sink_meta$closed)) close(sink_meta$stderr_con)
    }

    if (sink_meta$return_verbose) {
      stdout <- read_file(sink_meta$stdout_file)
      stderr <- read_file(sink_meta$stderr_file)
      if (sink_meta$verbose && length(stderr) > 0) {
        cat("Messages (not in order):\n")
        cat(paste(stderr, collapse = "\n"))
      }
    } else {
      stdout <- ""
      stderr <- ""
    }
  } else {
    stdout <- ""
    stderr <- ""
  }

  lst(stdout, stderr)
}
