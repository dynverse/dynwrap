#' Create a TI method from a docker / singularity container
#'
#' These functions create a TI method from a container using `babelwhale`. Supports both docker and singularity as a backend.
#'
#' @param container_id The name of the container repository (e.g. `"dynverse/ti_angle"`).
#' @param version The minimum required version of the TI method container.
#'   If the required version is higher than the currently installed version,
#'   the container will be pulled from dockerhub or singularityhub.
#' @param pull_if_needed Pull the container if not yet available.
#'
#' @importFrom babelwhale get_default_config pull_container
#'
#' @export
create_ti_container <- function(
  container_id,
  version = NULL,
  pull_if_needed = TRUE
) {
  config <- babelwhale::get_default_config()

  ######################################################
  ####           TEST DOCKER/SINGULARITY            ####
  ######################################################

  if (config$backend == "docker") {
    docker_installed <- test_docker_installation()
    if (!docker_installed) {
      test_docker_installation(detailed = TRUE)
    }
  } else if (config$backend == "singularity") {
    # TODO: there should be a test_singularity_installation()
  }

  ######################################################
  ####          FETCH CURRENT REPO DIGEST           ####
  ######################################################

  current_version <- .container_get_version(container_id)

  # pull if container can't be found
  if (pull_if_needed && identical(current_version, NA)) {
    babelwhale::pull_container(container_id)

    current_version <- .container_get_version(container_id)
  }

  ######################################################
  ####          PULL NEW IMAGE (IF NEEDED)          ####
  ######################################################

  if (config$backend != "singularity" || config$use_cache) {
    if (identical(current_version, NA) || (!is.null(version) && current_version < version)) {
      msg <- ifelse(identical(current_version, NA), "Container is not in cache", "Cache is out of date")
      message("Pulling container: '", container_id, "'. Reason: '", msg, "'. This might take a while.")

      babelwhale::pull_container(container_id)

      new_version <- .container_get_version(container_id)
      if (!is.null(version) && new_version < version) {
        warning("After pulling '", container_id, "', version number is lower than requested.\nCurrent version: ", new_version, ", expected >= ", version)
      }
    }
  }

  ######################################################
  ####               CREATE DEFINITION              ####
  ######################################################

  definition <- .container_get_definition(container_id)
  definition$run_info <- list(
    backend = "container",
    container_id = container_id
  )

  .method_process_definition(definition)
}




.method_execution_preproc_container <- function(
  run_info
) {
  # # process params
  # param_ids <- names(definition$parameters) %>% setdiff(c("forbidden"))
  #
  # # process required inputs
  # input_ids_required <- definition$input$required
  # testthat::expect_true(length(input_ids_required) > 0)
  # testthat::expect_true(is.character(input_ids_required))
  #
  # # process optional inputs
  # input_ids_optional <- if (!is.null(definition$input$optional)) definition$input$optional else character()
  # testthat::expect_true(is.character(input_ids_optional))
  #
  # # check input ids
  # input_ids <- c(input_ids_required, input_ids_optional)
  #
  # utils::data(allowed_inputs, package = "dynwrap", envir = environment())
  # if (!all(input_ids %in% allowed_inputs$input_id)) {
  #   stop("Invalid input: ", setdiff(input_ids, allowed_inputs$input_id), ". See dynwrap::allowed_inputs")
  # }
  #
  # # determine input format
  # if (length(definition$input$format) > 1) {
  #   message("Available input_formats are: ", glue::glue_collapse(definition$input$format, ", "), "; using first")
  # }
  # input_format <- definition$input$format[[1]]
  # testthat::expect_true(is.character(input_format))
  #
  # # process outputs
  # output_ids <- definition$output$outputs
  # testthat::expect_true(is.null(output_ids) || is.character(output_ids))
  #
  # # check output ids
  # utils::data(allowed_outputs, package = "dynwrap", envir = environment())
  # if (length(output_ids) && !all(output_ids %in% allowed_outputs$output_id)) {
  #   stop("Invalid output: ", setdiff(output_ids, allowed_outputs$output_id), ". See dynwrap::allowed_outputs")
  # }
  #
  # # determine output format
  # if (length(definition$output$format) > 1) {
  #   message("Available output_formats are: ", glue::glue_collapse(definition$output$format, ", "), ", using first")
  # }
  # output_format <- definition$output$format[[1]]
  # testthat::expect_true(is.character(output_format))

  dir_dynwrap <- dynutils::safe_tempdir("ti")

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
    dir_input = dir_input,
    input_format = input_format,
    input_ids = input_ids,
    param_ids = c(param_ids, "input_format", "output_format", "output_ids", "seed")
  )

  lst(
    dir_dynwrap
  )
}






.method_execution_execute_container <- function() {
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
  output <- babelwhale::run(
    container_id = container_id,
    command = NULL,
    args = NULL,
    volumes = paste0(dir_dynwrap %>% babelwhale:::fix_windows_path(), ":/ti"),
    workspace = "/ti/workspace",
    verbose = verbose,
    debug = debug
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




.method_execution_postproc_container <- function() {
  if (remove_files && !debug) {
    unlink(dir_dynwrap, recursive = TRUE)
  }
}

