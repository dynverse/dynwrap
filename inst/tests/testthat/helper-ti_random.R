#' Inferring trajectories with Control: random
#'
#' This control method will always produce a random trajectory.
#'
#' @param dummy_param This parameter does not do anything.
ti_random <- dynwrap::create_ti_method_r(
  dynwrap::definition(
    method = dynwrap::def_method(id = "random"),
    wrapper = dynwrap::def_wrapper(input_required = "counts")
  ),

  # describe packages needed by method
  package_loaded = c("dplyr", "tidyr", "purrr", "dynwrap", "dynutils"),
  package_required = c("dyndimred"),

  # function to run the method with
  run_fun = function(
    counts,
    seed = NA,
    verbose = FALSE
  ) {
    if (length(seed) > 0 && is.finite(seed)) set.seed(seed)

    num_milestones <- 15

    # generate network
    milestone_ids <- paste0("milestone_", seq_len(num_milestones))

    # TIMING: done with preproc
    tl <- add_timing_checkpoint(NULL, "method_afterpreproc")

    gr <- igraph::ba.game(num_milestones)
    milestone_network <- igraph::as_data_frame(gr) %>%
      mutate(
        from = paste0("milestone_", from),
        to = paste0("milestone_", to),
        length = 1,
        directed = FALSE
      )

    # put cells on random edges of network
    cell_ids <- rownames(counts)

    progressions <- data.frame(
      cell_id = cell_ids,
      milestone_network[sample.int(nrow(milestone_network), length(cell_ids), replace = TRUE), 1:2],
      percentage = stats::runif(length(cell_ids)),
      stringsAsFactors = FALSE
    )

    # TIMING: done with method
    tl <- tl %>% add_timing_checkpoint("method_aftermethod")

    # return output
    wrap_data(
      cell_ids = cell_ids
    ) %>% add_trajectory(
      milestone_ids = milestone_ids,
      milestone_network = milestone_network,
      progressions = progressions,
      divergence_regions = NULL
    ) %>% add_timings(
      timings = tl %>% add_timing_checkpoint("method_afterpostproc")
    )
  }
)
