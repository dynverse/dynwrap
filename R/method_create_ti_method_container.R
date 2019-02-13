#' Create a TI method from a docker / singularity container
#'
#' These functions create a TI method from a container using `babelwhale`. Supports both docker and singularity as a backend.
#'
#' @param container_id The name of the container repository (e.g. `"dynverse/ti_angle"`).
#' @param version The minimum required version of the TI method container.
#'   If the required version is higher than the currently installed version,
#'   the container will be pulled from dockerhub or singularityhub.
#' @param pull_if_needed Pull the container if not yet available.
#' @param return_function Whether to return a function that allows you to override the default parameters, or just return the method meta data as is.
#'
#' @importFrom babelwhale get_default_config pull_container test_docker_installation test_singularity_installation list_docker_images
#'
#' @export
create_ti_method_container <- function(
  container_id,
  version = NULL,
  pull_if_needed = TRUE,
  return_function = TRUE
) {
  config <- babelwhale::get_default_config()

  ######################################################
  ####           TEST DOCKER/SINGULARITY            ####
  ######################################################

  if (config$backend == "docker") {
    test_docker_installation()
  } else if (config$backend == "singularity") {
    test_singularity_installation()
  }

  ######################################################
  ####          FETCH CURRENT REPO DIGEST           ####
  ######################################################

  ######################################################
  ####          PULL NEW IMAGE (IF NEEDED)          ####
  ######################################################

  # TODO: only pull if container is not available
  babelwhale::pull_container(container_id)

  if (config$backend == "docker") {
    tab <- list_docker_images(container_id)

    if (nrow(tab) == 0) {
      babelwhale::pull_container(container_id)
    }
  } else if (config$backend == "singularity") {
    babelwhale::pull_container(container_id)
  }

  ######################################################
  ####               CREATE DEFINITION              ####
  ######################################################

  definition <- .container_get_definition(container_id)

  # check input format
  testthat::expect_true(is.character(definition$input$format))
  testthat::expect_equal(length(definition$input$format), 1)
  testthat::expect_true(definition$input$format %in% c("hdf5", "text", "rds"))

  # check available inputs
  testthat::expect_true(all(definition$input$required %in% dynwrap::allowed_inputs$input_id))
  testthat::expect_true(all(definition$input$optional %in% dynwrap::allowed_inputs$input_id))

  # check output format
  testthat::expect_true(is.character(definition$output$format))
  testthat::expect_equal(length(definition$output$format), 1)
  testthat::expect_true(definition$output$format %in% c("hdf5", "text", "rds", "dynwrap"))

  # check available outputs
  testthat::expect_true(all(definition$output$outputs %in% dynwrap::allowed_outputs$output_id))

  # save container info
  definition$run_info <- list(
    backend = "container",
    container_id = container_id
  )

  .method_process_definition(definition = definition, return_function = return_function)
}




.method_execution_preproc_container <- function(method, inputs, parameters, verbose, seed, debug) {
  dir_dynwrap <- dynutils::safe_tempdir("ti")

  # construct paths
  paths <- lst(
    dir_dynwrap,
    dir_input = file.path(dir_dynwrap, "input"),
    dir_output = file.path(dir_dynwrap, "output"),
    dir_workspace = file.path(dir_dynwrap, "workspace"),
    dir_tmp = file.path(dir_dynwrap, "tmp")
  )
  dir_input <- paths$dir_input

  # create all subdirectories
  walk(paths, dir.create, showWarnings = FALSE)

  # get formats
  input_format <- method$input$format
  output_format <- method$output$format

  # save data depending on the input_format
  if (input_format == "text") {
    walk2(inputs, names(inputs), function(input, input_id) {
      write_text_infer(input, glue::glue("{dir_input}/{input_id}"))
    })
  } else if (input_format == "rds") {
    write_rds(inputs, file.path(dir_input, "data.rds"))
  } else if (input_format == "hdf5") {
    # install hdf5r if not available
    dynutils::install_packages("hdf5r", "dynwrap", prompt = TRUE)
    requireNamespace("hdf5r")

    file <- hdf5r::H5File$new(file.path(dir_input, "data.h5"), "w")
    walk2(inputs, names(inputs), function(x, name) {
      file$create_dataset(name, x)

      if (is.matrix(x)) {
        file$create_dataset(paste0(name, "_rows"), rownames(x))
        file$create_dataset(paste0(name, "_cols"), colnames(x))
      }
    })
    file$close_all() # important to do a close_all here, otherwise some parts of the data can still be open, resulting in invalid h5 files
  }

  # save params as json
  parameters$verbose <- verbose
  parameters$seed <- seed
  parameters$input_format <- input_format
  parameters$output_format <- output_format

  jsonlite::write_json(parameters, file.path(dir_input, "params.json"), auto_unbox = TRUE)

  # return path information
  paths$debug <- debug
  paths$verbose <- verbose

  paths
}


.method_execution_execute_container <- function(method, preproc_meta) {
  # print information if desired
  if (preproc_meta$verbose) {
    cat(
      "Input saved to ", preproc_meta$dir_input, ": \n\t",
      paste(list.files(preproc_meta$dir_input), collapse = "\n\t"),
      "\n",
      sep = ""
    )
  }

  # run container
  output <- babelwhale::run(
    container_id = method$run_info$container_id,
    command = NULL,
    args = NULL,
    volumes = paste0(preproc_meta$dir_dynwrap %>% babelwhale:::fix_windows_path(), ":/ti"),
    workspace = "/ti/workspace",
    verbose = preproc_meta$verbose,
    debug = preproc_meta$debug
  )

  if (preproc_meta$verbose) {
    cat(
      "Output found in ", preproc_meta$dir_output, ": \n\t",
      paste(list.files(preproc_meta$dir_output), collapse = "\n\t"),
      "\n",
      sep = ""
    )
  }

  # wrap output
  model <-
    wrap_output(
      output_ids = method$output$outputs,
      dir_output = preproc_meta$dir_output,
      output_format =  method$output$format
    )

  # return output
  model
}




.method_execution_postproc_container <- function(preproc_meta) {
  if (!preproc_meta$debug) {
    unlink(preproc_meta$dir_dynwrap, recursive = TRUE)
  }
}



#' @importFrom utils write.csv
write_text_infer <- function(x, path) {
  if(is.matrix(x)) {
    utils::write.csv(x, paste0(path, ".csv"))
  } else if (is.data.frame(x)) {
    readr::write_csv(x, paste0(path, ".csv"))
  } else {
    jsonlite::write_json(x, paste0(path, ".json"))
  }
}
