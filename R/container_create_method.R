

#' @importFrom jsonlite write_json read_json
#' @importFrom utils data
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
  testthat::expect_true(is.character(definition$id))

  # parameters
  param_ids <- names(definition$parameters) %>% setdiff(c("forbidden"))

  # input
  input_ids_required <- definition$input$required
  testthat::expect_true(length(input_ids_required) > 0)
  testthat::expect_true(is.character(input_ids_required))

  input_ids_optional <- if (!is.null(definition$input$optional)) {definition$input$optional} else {character()}
  testthat::expect_true(is.character(input_ids_optional))

  input_ids <- c(input_ids_required, input_ids_optional)

  utils::data(allowed_inputs, package = "dynwrap", envir = environment())
  if (!all(input_ids %in% allowed_inputs$input_id)) {
    stop("Invalid input: ", setdiff(input_ids, allowed_inputs$input_id), ". See dynwrap::allowed_inputs")
  }

  if (length(definition$input$format) > 1) {
    message("Available input_formats are: ", glue::glue_collapse(definition$input$format, ", "), ", using first")
  }
  input_format <- definition$input$format[[1]]
  testthat::expect_true(is.character(input_format))

  # output
  output_ids <- definition$output$outputs
  testthat::expect_true(is.null(output_ids) || is.character(output_ids))

  utils::data(allowed_outputs, package = "dynwrap", envir = environment())
  if (length(output_ids) && !all(output_ids %in% allowed_outputs$output_id)) {
    stop("Invalid output: ", setdiff(output_ids, allowed_outputs$output_id), ". See dynwrap::allowed_outputs")
  }

  if (length(definition$output$format) > 1) {
    message("Available output_formats are: ", glue::glue_collapse(definition$output$format, ", "), ", using first")
  }
  output_format <- definition$output$format[[1]]
  testthat::expect_true(is.character(output_format))

  # define run_fun ------------------------------------------------------------------------
  definition$run_fun <- function(input_ids, param_ids, output_ids, run_container) {
    dir_dynwrap <- safe_tempdir("ti")

    if (remove_files && !debug) {
      on.exit(unlink(dir_dynwrap, recursive = TRUE))
    }

    # get names for directories
    dir_input <- file.path(dir_dynwrap, "input")
    dir_output <- file.path(dir_dynwrap, "output")

    # create directories
    dir.create(c(dir_input, dir_output, file.path(dir_dynwrap, c("workspace", "tmp"))))

    # save data & params, see save_inputs function
    save_inputs(environment(), dir_input, input_format, input_ids, c(param_ids, "input_format", "output_format", "output_ids"))

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
    output <- run_container(
      image,
      volumes = glue("{dir_dynwrap}:/ti"),
      debug,
      verbose
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

  # define run_container function
  if (image_type == "docker") {
    run_container <- function(image, volumes, debug, verbose) {
      if (debug) {
        requireNamespace("crayon")
        stop(
          "Use this command for debugging: \n",
          crayon::bold(
            glue::glue(
              "docker run --entrypoint 'bash' -e TMPDIR=/ti/tmp --workdir /ti/workspace -it {paste0(paste0('-v ', volumes), collapse = ' ')} {image}"
            )
          ),
        call. = FALSE)
      } else {
        processx::run(
          "docker",
          c("run", "-e", "TMPDIR=/ti/tmp", "--workdir", "/ti/workspace", as.character(rbind("-v", volumes)), image),
          echo = verbose,
          echo_cmd = verbose,
          spinner = TRUE
        )
      }
    }
  } else if (image_type == "singularity") {
    run_container <- function(image, volumes, debug, verbose) {
      if (!file.exists(image)) {
        stop(image, " not found!")
      }

      if (debug) {
        stop(
          "Use this command for debugging: \n",
          crayon::bold(
            glue::glue(
              "SINGULARITYENV_TMPDIR=/ti/tmp singularity exec --cleanenv --pwd /ti/workspace -B {glue::glue_collapse(volumes, ',')} {image} bash"
            )
          ),
        call. = FALSE)
      } else {
        stdout <- stderr <- if(verbose) {""} else {FALSE}

        # use system2 here instead of processx
        # processx has a strange bug that it doesn't show any output of the method
        # probably some problem with buffering and singularity
        stdout_file <- tempfile()
        output <- system2(
          "singularity",
          c("-s", "run", "--cleanenv", "--pwd", "/ti/workspace", "-B", glue::glue_collapse(volumes, ','), image),
          stdout = stdout_file,
          stderr = stdout_file,
          env = "SINGULARITYENV_TMPDIR=/ti/tmp"
        )
        cat(paste0(readLines(stdout_file), collapse = "\n"))

        if (output > 0) {
          stop(call. = FALSE)
        }

        list(
          status = output
        )

        # output <- processx::run(
        #   "singularity",
        #   c("-s", "run", "--cleanenv", "-B", glue::glue_collapse(volumes, ','), image),
        #   echo = verbose,
        #   echo_cmd = verbose,
        #   spinner = TRUE
        # )
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
      verbose = FALSE,
      remove_files = TRUE
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

  result <- processx::run("docker", c("inspect", "--type=image", image), error_on_status = FALSE)
  if (result$status > 0) {
    message("Image not found locally, pulling image... Stay tuned!")
    processx::run("docker", c("pull", image), echo = TRUE)
  }

  create_image_ti_method(image, definition, "docker", ...)
}

#' @rdname create_docker_ti_method
#'
#' @param definition_location The location of the definition file within the image
#'
#' @importFrom utils tail
#'
#' @export
extract_definition_from_docker_image <- function(
  image,
  definition_location = "/code/definition.yml"
) {
  requireNamespace("yaml")

  # start container
  output <- processx::run(
    "docker",
    c("create", "--entrypoint", "bash", image),
    stderr_callback = print_processx
  )
  id <- trimws(utils::tail(output$stdout, 1))

  # copy file from container
  definition_location_local <- tempfile()
  processx::run(
    "docker",
    c("cp", glue::glue("{id}:{definition_location}"), definition_location_local),
    stderr_callback = print_processx
  )

  # remove container
  processx::run("docker", c("rm", id), stderr_callback = print_processx)


  # read definition file
  definition <- yaml::read_yaml(definition_location_local)

  # add docker version id
  definition$docker_hash <- processx::run("docker", c("inspect", image, "--format", "{{.ID}}"))$stdout %>% trimws()

  definition
}

#' Creating a TI method from a docker repository
#'
#' Supports both docker and singularity as a backend.
#'
#' @param docker_repository The id of the docker repository.
#' @param run_environment In which environment to run the method, can be `"docker"` or `"singularity"`.
#' @param ... Default parameters for the method.
#' @export
create_container_ti_method <- function(docker_repository, run_environment = NULL, ...) {
  if (is.null(run_environment)) {
    run_environment <- getOption("dynwrap_run_environment")
  }
  if (is.null(run_environment)) {
    run_environment <- "docker"
  }

  # choose environments
  if (run_environment == "docker") {
    method <- create_docker_ti_method(docker_repository)
  } else if (run_environment == 'singularity') {
    method <- create_singularity_ti_method(docker_repository)
  }

  method(...)
}


#' @rdname create_docker_ti_method
#' @export
pull_docker_ti_method <- function(
  image
) {
  processx::run("docker", c("pull", image), stderr_callback = print_processx)

  create_docker_ti_method(image)
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
  singularity_images_folder = get_default_singularity_images_folder(),
  definition = extract_definition_from_singularity_image(image, singularity_images_folder),
  ...
) {
  singularity_image_location <- get_singularity_image_location(image, singularity_images_folder)
  create_image_ti_method(singularity_image_location, definition, "singularity", ...)
}


#' @rdname create_singularity_ti_method
#' @param definition_location The location of the definition file within the image
#'
#' @export
extract_definition_from_singularity_image <- function(
  image,
  singularity_images_folder = get_default_singularity_images_folder(),
  definition_location = "/code/definition.yml"
) {
  requireNamespace("yaml")

  singularity_image_location <- get_singularity_image_location(image, singularity_images_folder)

  definition_folder_local <- safe_tempdir("")
  processx::run(
    "singularity",
    c(
      "exec",
      "-B",
      glue("{definition_folder_local}:/tmp_definition"),
      singularity_image_location,
      "cp",
      definition_location,
      "/tmp_definition"
    ),
    stderr_callback = print_processx
  )

  definition_location_local <- file.path(definition_folder_local, basename(definition_location))

  definition <- yaml::read_yaml(definition_location_local)
  definition
}

#' @rdname create_singularity_ti_method
#' @param singularity_images_folder The location of the folder containing the singularity images. By default, this will use either the DYNWRAP_SINGULARITY_IMAGES_FOLDER environment variable, the dynwrap_singularity_images_folder option, or the working directory.
#' @param return_method Whether to return the method (TRUE) or only pull the image (FALSE)
#' @export
pull_singularity_ti_method <- function(
  image = image,
  singularity_images_folder = get_default_singularity_images_folder(),
  return_method = TRUE
) {
  singularity_image_location <- get_singularity_image_location(image, singularity_images_folder)
  dir.create(dirname(singularity_image_location), showWarnings = FALSE, recursive = TRUE)
  processx::run(
    "singularity",
    c(
      "build",
      singularity_image_location,
      glue("docker://{image}")
    ),
    echo = TRUE
  )

  if (return_method) {
    create_singularity_ti_method(image, singularity_images_folder)
  } else {
    NULL
  }
}


get_singularity_image_location <- function(
  image,
  singularity_images_folder = get_default_singularity_images_folder()
) {
  if (is.null(singularity_images_folder)) {


    singularity_images_folder <- "."
  }

  # add simg if missing
  if (tools::file_ext(image) == "") {
    image <- paste0(image, ".simg")
  }

  normalizePath(paste0(singularity_images_folder, "/", image), mustWork = F)
}

get_default_singularity_images_folder <- function() {
  if (Sys.getenv("DYNWRAP_SINGULARITY_IMAGES_FOLDER") != "") {
    Sys.getenv("DYNWRAP_SINGULARITY_IMAGES_FOLDER")
  } else if (!is.null(getOption("dynwrap_singularity_images_folder"))) {
    getOption("dynwrap_singularity_images_folder")
  } else {
    warning("No 'singularity_image_folder' specified, 'dynwrap_singularity_images_folder' option not set and 'DYNWRAP_SINGULARITY_IMAGES_FOLDER' environment variable not set. Putting images in working directory.")
    "./"
  }
}
