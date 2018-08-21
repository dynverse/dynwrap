#' Inferring trajectories with Component 1
#'
#' Wrapper around TI method
#'
#' @param dimred A character vector specifying which dimensionality reduction method to use.
#'   See [dyndimred::dimred] for the list of available dimensionality reduction methods.
#' @param component The component to use
#' @inheritParams dyndimred::dimred
#'
#' @include method_create_ti_method.R
ti_comp1 <- create_ti_method(
  id = "comp1",
  package_loaded = c("tidyverse", "dynwrap", "dynutils"),
  package_required = c("dyndimred"),
  parameters = list(
    dimred = list(
      type = "discrete",
      default = "pca",
      values = names(dyndimred::list_dimred_methods())
    ),
    ndim = list(
      type = "integer",
      default = 2,
      lower = 2,
      upper = 30
    ),
    component = list(
      type = "integer",
      default = 1,
      lower = 1,
      upper = 10
    )
  ),
  run_fun = function(
    expression,
    ndim,
    dimred,
    component
  ) {
    # TIMING: done with preproc
    tl <- add_timing_checkpoint(NULL, "method_afterpreproc")

    space <- dyndimred::dimred(expression, method = dimred, ndim = ndim)

    # TIMING: done with method
    tl <- tl %>% add_timing_checkpoint("method_aftermethod")

    # return output
    wrap_prediction_model(
      cell_ids = rownames(expression)
    ) %>% add_linear_trajectory(
      pseudotime = space[,component] %>% set_names(rownames(expression))
    ) %>% add_dimred(
      dimred = space
    ) %>% add_timings(
      timings = tl %>% add_timing_checkpoint("method_afterpostproc")
    )
  }
)
