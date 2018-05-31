#' An abstract data wrapper for TI predictions
#'
#' @inheritParams wrap_data
#'
#' @export
wrap_prediction_model <- function(
  cell_ids,
  cell_info = NULL,
  ...
) {
  out <- wrap_data(
    id = random_time_string("TIpred"),
    cell_ids = cell_ids,
    cell_info = cell_info,
    ...
  )
  class(out) <- c("dynwrap::prediction", class(out))
  out
}

#' Tests whether an object is a trajectory created by a TI method.
#'
#' @param object The object to be tested
#'
#' @export
is_prediction <- function(object) {
  "dynwrap::prediction" %in% class(object)
}
