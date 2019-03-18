#' Inferring trajectories with Control: identity
#'
#' This control method will return the gold standard.
#'
#' @param dummy_param This parameter does not do anything.
ti_identity <- dynwrap::create_ti_method_r(
  dynwrap::definition(
    method = dynwrap::def_method(id = "identity"),
    wrapper = dynwrap::def_wrapper(input_required = c("counts", "dataset"))
  ),

  # describe packages needed by method
  package_loaded = c("dplyr", "tidyr", "purrr", "dynwrap", "dynutils"),

  # function to run the method with
  run_fun = function(
    counts,
    priors,
    seed = NA,
    verbose = FALSE
  ) {
    dataset <- priors$dataset

    if (length(seed) > 0 && is.finite(seed)) set.seed(seed)

    # TIMING: done with preproc
    tl <- add_timing_checkpoint(NULL, "method_afterpreproc")

    # TIMING: done with method
    tl <- tl %>% add_timing_checkpoint("method_aftermethod")

    # return output
    wrap_data(
      cell_ids = dataset$cell_ids,
      cell_info = dataset$cell_info
    ) %>% add_trajectory(
      milestone_ids = dataset$milestone_ids,
      milestone_network = dataset$milestone_network,
      divergence_regions = dataset$divergence_regions,
      progressions = dataset$progressions
    ) %>% add_timings(
      timings = tl %>% add_timing_checkpoint("method_afterpostproc")
    )
  }
)
