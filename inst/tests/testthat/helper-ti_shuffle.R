#' Inferring trajectories with Control: shuffle
#'
#' This control method will return the milestone network of the provided
#' gold standard, but will shuffle the cell positions randomly.
#'
#' @param dummy_param This parameter does not do anything.
ti_shuffle <- dynwrap::create_ti_method_r(
  dynwrap::definition(
    method = dynwrap::def_method(id = "shuffle"),
    wrapper = dynwrap::def_wrapper(input_required = c("counts", "dataset"))
  ),

  # describe packages needed by method
  package_loaded = c("dplyr", "tidyr", "purrr", "dynwrap", "dynutils"),
  package_required = c("dyndimred"),

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

    # permute cell labels
    allcells <- rownames(counts)
    mapper <- magrittr::set_names(sample(allcells), allcells)
    progressions <- dataset$progressions %>% mutate(
      cell_id = mapper[cell_id]
    )

    # TIMING: done with method
    tl <- tl %>% add_timing_checkpoint("method_aftermethod")

    # return output
    wrap_data(
      cell_ids = dataset$cell_ids
    ) %>% add_trajectory(
      milestone_ids = dataset$milestone_ids,
      milestone_network = dataset$milestone_network,
      progressions = progressions,
      divergence_regions = dataset$divergence_regions
    ) %>% add_timings(
      timings = tl %>% add_timing_checkpoint("method_afterpostproc")
    )
  }
)

