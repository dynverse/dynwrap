.method_execute <- function(
  dataset,
  method,
  parameters,
  give_priors,
  verbose,
  seed,
  capture_output = TRUE,
  debug = FALSE
  remove_files = TRUE
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

  # print helpful message
  if (verbose) {
    cat(
      "Executing '", method$id, "' on '", dataset$id, "'\n",
      "With parameters: ", deparse(parameters), "\n",
      "And inputs: ", paste0(names(inputs), collapse = ", "), "\n",
      sep = ""
    )
  }

  # run preproc
  preproc_meta <-
    if (run_info$backend == "function") {
      .method_execute_preproc_function(method = method, capture_output = capture_output)
    } else {
      .method_execute_preproc_container(method = method, inputs = inputs, parameters = parameters, verbose = verbose, seed = seed, debug = debug, remove_files = remove_files)
    }

  # run the method and catch the error, if necessary
  out <- tryCatch({
    # measure second time point
    time_start <- as.numeric(Sys.time())

    # execute method and return model
    if (run_info$backend == "function") {
      .method_execute_run_function(method = method, inputs = inputs, parameters = parameters, verbose = verbose, seed = seed, preproc_meta = preproc_meta)
    } else {
      .method_execute_run_container(method = method, preproc_meta = preproc_meta)
    }

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

  # run postproc
  postproc_meta <-
    if (run_info$backend == "function") {
      .method_execute_postproc_function(preproc_meta = preproc_meta)
    } else {
      .method_execute_postproc_container(preproc_meta = preproc_meta)
    }

  # retrieve the model, error message, and timings
  model <- out$model
  error <- out$error
  timings_list <- out$timings_list

  # stop the timer
  time3 <- as.numeric(Sys.time())

  timings_list <- map(timings_list, as.numeric)

  # add missing timings
  missing_timing_ids <- setdiff(
    c("method_start", "method_afterpreproc", "method_aftermethod", "method_afterpostproc", "method_stop"),
    names(timings_list)
  )
  timings_list[missing_timing_ids] <- time3

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
    stdout = postproc_meta$stdout,
    stderr = postproc_meta$stderr,
    prior_df = list(method$inputs %>% rename(prior_id = input_id) %>% mutate(given = prior_id %in% names(args)))
  )

  lst(model, summary)
}
