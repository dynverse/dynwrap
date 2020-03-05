#' Inferring trajectories with Component 1
#'
#' Wrapper around TI method
#'
#' @param component The component to use
ti_comp1 <- dynwrap::create_ti_method_r(
  definition = dynwrap::definition(
    # describe method
    dynwrap::def_method(
      id = "comp1"
    ),

    # describe tuneable parameters
    parameters = dynparam::parameter_set(
      dynparam::integer_parameter(
        id = "component",
        default = 1,
        distribution = dynparam::uniform_distribution(1L, 10L)
      )
    ),

    # describe wrapper
    wrapper = dynwrap::def_wrapper(
      # describe run fun inputs and outputs
      input_required = "expression",
      input_optional = NULL
    )

  ),

  # function to run the method with
  run_fun = function(
    expression,
    parameters,
    seed = NA,
    verbose = FALSE
  ) {
    if (length(seed) > 0 && is.finite(seed)) set.seed(seed)

    # TIMING: done with preproc
    tl <- add_timing_checkpoint(NULL, "method_afterpreproc")

    dimred <- prcomp(as.matrix(expression), rank. = 10)$x
    pseudotime <- dimred[,parameters$component]
    names(pseudotime) <- rownames(expression)

    # TIMING: done with method
    tl <- tl %>% add_timing_checkpoint("method_aftermethod")

    # return output
    wrap_data(
      cell_ids = rownames(expression)
    ) %>% add_linear_trajectory(
      pseudotime = pseudotime
    ) %>% add_dimred(
      dimred = dimred
    ) %>% add_timings(
      timings = tl %>% add_timing_checkpoint("method_afterpostproc")
    )
  },

  # describe packages needed by method
  package_loaded = c("dplyr", "tidyr", "purrr", "dynwrap", "dynutils"),
  package_required = NULL
)
