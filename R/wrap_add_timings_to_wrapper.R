#' Add count and normalised expression values to a data wrapper
#'
#' @param data_wrapper A data wrapper to extend upon.
#' @param timings A list of timings.
#'
#' @export
#'
#' @importFrom testthat expect_equal
add_timings_to_wrapper <- function(
  data_wrapper,
  timings
) {
  testthat::expect_is(data_wrapper, "dynutils::data_wrapper")

  testthat::expect_is(timings, "list")

  # create output structure
  out <- c(
    data_wrapper,
    list(timings = timings)
  )
  class(out) <- c("dynutils::with_timings", class(data_wrapper))
  out
}

#' Test whether an object is a data_wrapper and has timings information
#'
#' @param object The object to be tested.
#'
#' @export
is_wrapper_with_timings <- function(object) {
  is_data_wrapper(object) && "dynutils::with_timings" %in% class(object)
}

#' Helper function for storing timings information.
#'
#' @param timings The timings list of previous checkpoints.
#' @param name The name of the timings checkpoint.
#'
#' @export
add_timing_checkpoint <- function(timings, name) {
  if (is.null(timings)) {
    timings <- list()
  }
  timings[[name]] <- Sys.time()
  timings
}
