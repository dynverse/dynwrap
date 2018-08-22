#' Inferring trajectories with Control: identity
#'
#' This control method will return the gold standard.
#'
#' @param dummy_param This parameter does not do anything.
ti_identity <- create_ti_method(
  id = "identity",
  package_loaded = c("dplyr", "tidyr", "purrr", "dynwrap", "dynutils"),
  package_required = c("dyndimred"),
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
    dataset,
    dummy_param = .5
  ) {
    # TIMING: done with preproc
    tl <- add_timing_checkpoint(NULL, "method_afterpreproc")

    # TIMING: done with method
    tl <- tl %>% add_timing_checkpoint("method_aftermethod")

    # return output
    wrap_prediction_model(
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
