#' Inferring trajectories with Control: error
#'
#' This control method will always produce an error.
#'
#' @param dummy_param This parameter does not do anything.
ti_error <- dynwrap::create_ti_method(
  id = "error",
  package_loaded = c(),
  package_required = c(),
  parameters = list(
    dummy_param = list(
      type = "numeric",
      default = 0.5,
      upper = 1,
      lower = 0,
      description = "Dummy parameter")
  ),
  run_fun = function(
    counts,
    dummy_param,
    seed = NA
  ) {
    stop("This control method always errors.")
  }
)

