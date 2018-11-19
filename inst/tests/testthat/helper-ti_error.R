#' Inferring trajectories with Control: error
#'
#' This control method will always produce an error.
ti_error <- dynwrap::create_ti_method_r(
  id = "error",

  # describe packages needed by method
  package_loaded = c(),
  package_required = c(),

  # describe run fun inputs and outputs
  input_required = "counts",
  input_optional = NULL,
  output = NULL,

  # describe tuneable parameters
  parameters = list(),

  # function to run the method with
  run_fun = function(
    counts,
    seed = NA,
    verbose = FALSE
  ) {
    stop("This control method always errors.")
  }
)

