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
#' # make two temporary files
#' definition.yml <- "~/test.yml" #tempfile()
#' run.R <- "~/test.R" #tempfile()
#'
#' # determine content
#' definition_txt <- "method:
#'   id: comp_1
#'
#' parameters:
#'   - id: component
#'     default: 1
#'     type: integer
#'     distribution:
#'       type: uniform
#'       lower: 1
#'       upper: 10
#'     description: The nth component to use
#'
#' wrapper:
#'   input_required: expression
#'   input_optional: start_id"
#'
#' # Normally, use #'/usr/bin/env Rscript as shebang
#' # but $R_HOME/bin/Rscript when running R CMD check
#' shebang <-
#'   if (Sys.getenv("_R_CHECK_PACKAGE_NAME_") == "") {
#'     "#!/usr/bin/env Rscript"
#'   } else {
#'     paste0("#!", Sys.getenv("R_HOME"), "/bin/Rscript")
#'   }
#' cat(shebang, "\n", sep = "")
#' run_txt <- paste0(shebang, "
#'
#' dataset <- dyncli::main()
#'
#' library(dynwrap)
#' library(dplyr)
#' library(stats)
#' library(dyncli)
#'
#' # infer trajectory
#' pca <- prcomp(dataset$expression)
#'
#' pseudotime <- pca$x[, dataset$parameters$component]
#'
#' # flip pseudotimes using start_id
#' if (!is.null(dataset$priors$start_id)) {
#'   if (mean(pseudotime[start_id]) > 0.5) {
#'     pseudotime <- 1 - pseudotime
#'   }
#' }
#'
#' # build trajectory
#' trajectory <- wrap_data(cell_ids = rownames(dataset$expression)) %>%
#'   add_linear_trajectory(pseudotime = pseudotime)
#'
#' # save output
#' write_output(trajectory, dataset$output)")
#'
#' # write content to files
#' writeLines(definition_txt, definition.yml)
#' writeLines(run_txt, run.R)
#'
#' requireNamespace("fs")
#' fs::file_chmod(run.R, "+x")
#'
#' # run tmethod
#' method <- create_ti_method_definition(
#'   definition.yml,
#'   run.R
#' )
#' trajectory <- infer_trajectory(example_dataset, method(), verbose = TRUE)
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
