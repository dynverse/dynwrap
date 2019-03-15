#' Inferring trajectories with Control: error
#'
#' This control method will always produce an error.
ti_error <- dynwrap::create_ti_method_r(
  dynwrap::definition(
    method = dynwrap::def_method(id = "error"),
    wrapper = dynwrap::def_wrapper(input_required = "counts")
  ),

  # function to run the method with
  run_fun = function(
    counts,
    seed = NA,
    verbose = FALSE
  ) {
    stop("This control method always errors.")
  }
)

