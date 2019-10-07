#' Create a TI method from a local method definition file
#'
#' The local method definition file describes a method that is runnable on the local system.
#' See [`vignette("create_ti_method_definition", "dynwrap")`](../doc/create_ti_method_definition.html) for a tutorial on how to create a containerized TI method.
#'
#' @param script Location of the script that will be executed. Has to contain a #!
#' @inheritParams .method_process_definition
#'
#' @inherit create_ti_method_container return
#'
#' @keywords create_ti_method
#'
#' @examples
#' \donttest{
#' method <- create_ti_method_definition(
#'   system.file("examples/script/definition.yml", package = "dynwrap"),
#'   system.file("examples/script/run.R", package = "dynwrap")
#' )
#' trajectory <- infer_trajectory(example_dataset, method())
#' }
#'
#' @importFrom yaml read_yaml
#'
#' @export
create_ti_method_definition <- function(
  definition,
  script,
  return_function = TRUE
) {
  definition_path <- normalizePath(definition)
  definition <- .method_load_definition(definition)

  if (!is.null(script)) script <- normalizePath(script)

  definition$run <- list(
    backend = "script",
    script = script,
    definition = definition_path
  )

  .method_process_definition(definition = definition, return_function = return_function)
}


.method_execution_execute_script <- function(method, preproc_meta) {
  # copy over script and rds
  file.copy(normalizePath(method$run$script), preproc_meta$dir_dynwrap)
  script_location <- basename(method$run$script)
  file.copy(normalizePath(method$run$definition), paste0(preproc_meta$dir_dynwrap, "/definition.yml"))

  # print information if desired
  if (preproc_meta$verbose) {
    cat("Input saved to ", preproc_meta$dir_dynwrap, "\n", sep = "")
  }

  # run script
  command <- paste0("./", script_location)
  args <- c("--dataset", "input.h5", "--output", "output.h5")
  if (preproc_meta$debug) args <- c(args, "--debug")

  process <- processx::run(
    command = command,
    args = args,
    wd = preproc_meta$dir_dynwrap,
    echo = as.logical(preproc_meta$verbose) || preproc_meta$debug,
    echo_cmd = as.logical(preproc_meta$verbose) || preproc_meta$debug,
    spinner = TRUE
  )

  # return output
  dynutils::read_h5(file.path(preproc_meta$dir_dynwrap, "output.h5"))
}
