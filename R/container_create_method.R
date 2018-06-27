# get mountable temporary directory
# on osx, the R temporary directory is placed in the /var folder, but this is not standard accessibale for docker
# in that case, we put it in /tmp
mytempdir <- function(subfolder) {
  dir <- file.path(tempdir(), subfolder) %>% gsub("^/var/", "/tmp/", .)
  if (dir.exists(dir)) {
    unlink(dir, recursive = TRUE, force = TRUE)
  }
  dir.create(dir, recursive = TRUE)

  dir
}

#' @importFrom jsonlite write_json read_json
create_image_ti_method <- function(
  image,
  definition,
  image_type = c("docker", "singularity"),
  plot_fun = NULL
) {
  image_type <- match.arg(image_type)

  # some checking of definition file -----------------------------------------------------
  # name
  testthat::expect_true(is.character(definition$name))

  testthat::expect_true(is.character(definition$short_name) || is.null(definition$short_name))

  # parameters
  param_ids <- names(definition$parameters) %>% setdiff(c("forbidden"))

  # input
  input_ids_required <- definition$input$required
  testthat::expect_true(length(input_ids_required) > 0)
  testthat::expect_true(is.character(input_ids_required))

  input_ids_optional <- if (!is.null(definition$input$optional)) {definition$input$optional} else {character()}
  testthat::expect_true(is.character(input_ids_optional))

  input_ids <- c(input_ids_required, input_ids_optional)

  data(allowed_inputs, package = "dynwrap", envir = environment())
  if (!all(input_ids %in% allowed_inputs$input_id)) {
    stop("Invalid input: ", setdiff(input_ids, allowed_inputs$input_id), ". See dynwrap::allowed_inputs")
  }

  if (length(definition$input$format) > 1) {
    message("Available input_formats are: ", glue::collapse(definition$input$format, ", "), ", using first")
  }
  input_format <- definition$input$format[[1]]
  testthat::expect_true(is.character(input_format))

  # output
  output_ids <- definition$output$outputs
  testthat::expect_true(is.null(output_ids) || is.character(output_ids))

  data(allowed_outputs, package = "dynwrap", envir = environment())
  if (length(output_ids) && !all(output_ids %in% allowed_outputs$output_id)) {
    stop("Invalid output: ", setdiff(output_ids, allowed_outputs$output_id), ". See dynwrap::allowed_outputs")
  }

  if (length(definition$output$format) > 1) {
    message("Available output_formats are: ", glue::collapse(definition$output$format, ", "), ", using first")
  }
  output_format <- definition$output$format[[1]]
  testthat::expect_true(is.character(output_format))

  # define run_fun ------------------------------------------------------------------------
  definition$run_fun <- function(input_ids, param_ids, output_ids, run_container) {
    # create input directory
    dir_input <- mytempdir("input")

    # save data & params, see save_inputs function
    save_inputs(environment(), dir_input, input_format, input_ids, c(param_ids, "input_format", "output_format"))

    if (verbose) {
      # print provided input files
      requireNamespace("crayon")
      list.files(dir_input) %>%
        crayon::bold() %>%
        glue::collapse("\n\t") %>%
        paste0("input saved to ", dir_input, ": \n\t", ., "\n") %>%
        cat
    }


    # create output directory
    dir_output <- mytempdir("output")

    # run container
    status <- run_container(
      image,
      volumes = c(
        glue("{dir_input}:/input"),
        glue("{dir_output}:/output")
      ),
      debug
    )

    # exit if error
    if (status != 0) {
      stop(call. = FALSE)
    }

    if (verbose) {
      # print found output files
      requireNamespace("crayon")
      list.files(dir_output) %>%
        crayon::bold() %>%
        glue::collapse("\n\t") %>%
        paste0("output saved in ", dir_output, ": \n\t", ., "\n") %>%
        cat
    }

    # wrap output
    if("counts" %in% input_ids) {
      cell_ids <- rownames(counts)
    } else {
      cell_ids <- rownames(expression)
    }

    model <- wrap_data(cell_ids = cell_ids) %>%
      wrap_output(output_ids, dir_output, output_format)

    # add timing
    if(!is.null(model$timings)) {
      model$timings$method_afterpostproc <- as.numeric(Sys.time())
    }

    model
  }

  # define run_container function
  if (image_type == "docker") {
    run_container <- function(image, volumes, debug) {
      if (debug) {
        requireNamespace("crayon")
        stop(
          "Use this command for debugging: \n",
          crayon::bold(
            glue::glue(
              "docker run --entrypoint 'bash' -it {paste0(paste0('-v ', volumes), collapse = ' ')} {image}"
            )
          ),
        call. = FALSE)
      } else {
        system(glue::glue("docker run {paste0(paste0('-v ', volumes), collapse = ' ')} {image}"))
      }
    }
  } else if (image_type == "singularity") {
    run_container <- function(image, volumes, debug) {
      if (debug) {
        stop(
          "Use this command for debugging: \n",
          crayon::bold(
            glue::glue(
              "singularity exec -B {glue::collapse(volumes, ',')} {image} bash"
            )
          ),
        call. = FALSE)
      } else {
        system(glue("singularity run -B {glue::collapse(volumes, ',')} {image}"))
      }
    }
  }

  # adapt run_fun environment
  environment(definition$run_fun) <- list2env(lst(input_format, input_ids, param_ids, output_format, output_ids, run_container))

  # adapt run_fun arguments, some hocus pocus going on here ;)
  # the `list(expr())` creates a required function argument
  argument_ids <- c(input_ids, param_ids)
  arguments <- c(
    rep(list(expr()), length(input_ids_required)) %>% set_names(input_ids_required),
    rep(list(expr()), length(param_ids)) %>% set_names(param_ids),
    rep(list(NULL), length(input_ids_optional)) %>% set_names(input_ids_optional),
    alist(
      debug = FALSE,
      verbose = FALSE
    ) # default arguments (evaluated when running run_fun)
  )
  formals(definition$run_fun) <- arguments

  do.call(create_ti_method, definition)
}


#' Create a TI method from a docker image
#'
#' These functions create a TI method from a docker image, either locally (`create_docker_ti_method`) or from [docker hub](https://hub.docker.com/)  `pull_docker_ti_method`.
#'
#' @param image The name of the docker image, eg. `dynverse/comp1`. Can contain tags such as `dynverse/comp1:R_feather`
#' @param definition The method definition, a list containing the name, input, output and parameters of a method.
#'   Optional, as the definition file will be automatically loaded from the images `/code/definition.yml` using [extract_definition_from_docker_image].
#' @param ... Other information about the method
#'
#' @export
create_docker_ti_method <- function(
  image,
  definition = extract_definition_from_docker_image(image),
  ...
) {
  docker_installed <- test_docker_installation()
  if (!docker_installed) test_docker_installation(detailed = TRUE)

  tryCatch(
    system(glue::glue("docker inspect --type=image {image}"), intern = TRUE),
    warning = function(x) {
      message("Image not found locally, pulling image... Stay tuned!")
      system(glue::glue("docker pull {image}"))
      }
  )

  create_image_ti_method(image, definition, "docker", ...)
}

#' Create a TI method from a singularity image
#'
#' This function creates a TI method from a singularity image. This image can be build using `singularity build` from a docker image (eg. `singularity build comp1.simg docker://dynverse/comp1`)
#'
#' @param image The location of the singularity image file, eg. `comp1.simg`.
#' @param definition The method definition, a list containing the name, input, output and parameters of a method.
#'  Optional, as the definition file will be automatically loaded from the images `/code/definition.yml` using [extract_definition_from_singularity_image].
#' @param ... Other information about the method
#'
#' @export
create_singularity_ti_method <- function(
  image,
  definition = extract_definition_from_singularity_image(image),
  ...
) {
  # get absolutate path to image
  image <- normalizePath(image)

  create_image_ti_method(image, definition, "singularity", ...)
}

#' @rdname create_docker_ti_method
#'
#' @param definition_location The location of the definition file within the image
#'
#' @export
extract_definition_from_docker_image <- function(
  image,
  definition_location = "/code/definition.yml"
) {
  requireNamespace("yaml")

  # start container
  output <- system(glue::glue("docker create --entrypoint='bash' {image}"), intern = TRUE)
  id <- output[length(output)]
  if (!stringr::str_detect(id, "[A-Za-z0-9]*")) {stop("Docker errored ", output)}

  # copy file from container
  definition_location_local <- tempfile()
  system(glue::glue("docker cp {id}:{definition_location} {definition_location_local}"))

  # remove container
  system(glue::glue("docker rm {id}"))

  # read definition file
  definition <- yaml::read_yaml(definition_location_local)
  definition
}

#' @rdname create_singularity_ti_method
#' @param definition_location The location of the definition file within the image
#'
#' @export
extract_definition_from_singularity_image <- function(
  image,
  definition_location = "/code/definition.yml"
) {
  requireNamespace("yaml")

  definition_folder_local <- mytempdir("")
  system(glue("singularity exec -B {definition_folder_local}:/tmp_definition {image} cp {definition_location} /tmp_definition"))

  definition_location_local <- file.path(definition_folder_local, basename(definition_location))

  definition <- yaml::read_yaml(definition_location_local)
  definition
}

#' @rdname create_docker_ti_method
#' @export
pull_docker_ti_method <- function(
  image
) {
  system(glue::glue("docker pull {image}"))

  create_docker_ti_method(image)
}


pull_singularity_ti_method <- function(
  image,
  singularity_images_folder = options("dynwrap_singularity_")
) {

}
