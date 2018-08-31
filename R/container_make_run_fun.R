.container_make_run_fun <- function(
  definition,
  image
) {
  # process params
  param_ids <- names(definition$parameters) %>% setdiff(c("forbidden"))

  # process required inputs
  input_ids_required <- definition$input$required
  testthat::expect_true(length(input_ids_required) > 0)
  testthat::expect_true(is.character(input_ids_required))

  # process optional inputs
  input_ids_optional <- if (!is.null(definition$input$optional)) definition$input$optional else character()
  testthat::expect_true(is.character(input_ids_optional))

  # check input ids
  input_ids <- c(input_ids_required, input_ids_optional)

  utils::data(allowed_inputs, package = "dynwrap", envir = environment())
  if (!all(input_ids %in% allowed_inputs$input_id)) {
    stop("Invalid input: ", setdiff(input_ids, allowed_inputs$input_id), ". See dynwrap::allowed_inputs")
  }

  # determine input format
  if (length(definition$input$format) > 1) {
    message("Available input_formats are: ", glue::glue_collapse(definition$input$format, ", "), "; using first")
  }
  input_format <- definition$input$format[[1]]
  testthat::expect_true(is.character(input_format))

  # process outputs
  output_ids <- definition$output$outputs
  testthat::expect_true(is.null(output_ids) || is.character(output_ids))

  # check output ids
  utils::data(allowed_outputs, package = "dynwrap", envir = environment())
  if (length(output_ids) && !all(output_ids %in% allowed_outputs$output_id)) {
    stop("Invalid output: ", setdiff(output_ids, allowed_outputs$output_id), ". See dynwrap::allowed_outputs")
  }

  # determine output format
  if (length(definition$output$format) > 1) {
    message("Available output_formats are: ", glue::glue_collapse(definition$output$format, ", "), ", using first")
  }
  output_format <- definition$output$format[[1]]
  testthat::expect_true(is.character(output_format))

  # create function
  run_fun <- function(..., debug, verbose, remove_files) {
    dir_dynwrap <- safe_tempdir("ti")

    if (remove_files && !debug) {
      on.exit(unlink(dir_dynwrap, recursive = TRUE))
    }

    # get paths for directories
    dir_input <- file.path(dir_dynwrap, "input")
    dir_output <- file.path(dir_dynwrap, "output")

    # create directories
    dir.create(dir_input)
    dir.create(dir_output)
    dir.create(file.path(dir_dynwrap, "workspace"))
    dir.create(file.path(dir_dynwrap, "tmp"))

    # save data & params, see save_inputs function
    .container_save_inputs(
      envir = environment(),
      dir_input = dir_input %>% fix_windows_path(),
      input_format = input_format,
      input_ids = input_ids,
      param_ids = c(param_ids, "input_format", "output_format", "output_ids")
    )

    if (verbose) {
      # print provided input files
      requireNamespace("crayon")
      list.files(dir_input) %>%
        crayon::bold() %>%
        glue::glue_collapse("\n\t") %>%
        paste0("Input saved to ", dir_input, ": \n\t", ., "\n") %>%
        cat
    }

    # run container
    output <- .container_run(
      image,
      command = NULL,
      extra_args = NULL,
      debug = debug,
      verbose = verbose,
      volumes = paste0(dir_dynwrap, ":/ti"),
      config = container_config(),
      workspace = "/ti/workspace"
    )

    # exit if error
    if (output$status != 0) {
      stop(call. = FALSE)
    }

    if (verbose) {
      # print found output files
      requireNamespace("crayon")
      list.files(dir_output) %>%
        crayon::bold() %>%
        glue::glue_collapse("\n\t") %>%
        paste0("output saved in ", dir_output, ": \n\t", ., "\n") %>%
        cat
    }

    # wrap output
    model <-
      wrap_output(output_ids, dir_output, output_format)

    # add timing
    if(!is.null(model$timings)) {
      model$timings$method_afterpostproc <- as.numeric(Sys.time())
    }

    model
  }

  # adapt run_fun environment
  environment(run_fun) <- list2env(lst(
    input_format,
    input_ids,
    param_ids,
    output_format,
    output_ids,
    image
  ))

  # adapt run_fun arguments, some hocus pocus going on here ;)
  # the `list(expr())` creates a required function argument
  argument_ids <- c(input_ids, param_ids)

  arguments <- c(
    rep(list(expr()), length(input_ids_required)) %>% set_names(input_ids_required),
    rep(list(expr()), length(param_ids)) %>% set_names(param_ids),
    rep(list(NULL), length(input_ids_optional)) %>% set_names(input_ids_optional),
    alist(
      debug = FALSE,
      verbose = FALSE,
      remove_files = TRUE
    ) # default arguments (evaluated when running run_fun)
  )

  formals(run_fun) <- arguments

  run_fun
}
