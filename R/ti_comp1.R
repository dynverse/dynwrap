#' Inferring trajectories with Component 1
#'
#' Wrapper around TI method
#'
#' @param dimred A character vector specifying which dimensionality reduction method to use.
#'   See \code{\link{dyndimred::list_dimred_methods}} for the list of available dimensionality reduction methods.
#' @inheritParams dyndimred::dimred
#'
#' @export
#'
#' @include create_ti_method.R
ti_comp1 <- create_ti_method(
  name = "Component 1",
  short_name = "comp1",
  package_loaded = c(),
  package_required = c(),
  par_set = ParamHelpers::makeParamSet(
    ParamHelpers::makeDiscreteParam(id = "dimred", default = "pca", values = names(dyndimred::list_dimred_methods())),
    ParamHelpers::makeIntegerParam(id="ndim", default = 2, lower=2, upper=30)
  ),
  run_fun = "run_comp1",
  plot_fun = "plot_comp1"
)

run_comp1 <- function(
  expression,
  ndim,
  dimred
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
    pseudotimes = space[,1] %>% setNames(rownames(expression))
  ) %>% add_dimred(
    dimred = space
  ) %>% add_timings(
    timings = tl %>% add_timing_checkpoint("method_afterpostproc")
  )
}

#' @importFrom viridis scale_colour_viridis
plot_comp1 <- function(prediction) {
  g <- ggplot() +
    geom_point(aes(Comp1, Comp2, color = Comp1), data.frame(prediction$dimred)) +
    viridis::scale_colour_viridis(option = "plasma") +
    labs(colour = "Pseudotime") +
    theme(legend.position = c(.92, .12))
  process_dynplot(g, prediction$id)
}
