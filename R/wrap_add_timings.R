#' Add count and normalised expression values to a model
#'
#' @param model The model to which timings will be added
#' @param timings A list of timings.
#'
#' @export
#'
#' @importFrom testthat expect_equal
add_timings <- function(
  model,
  timings
) {
  testthat::expect_true(is_data_wrapper(model))

  if (is.data.frame(timings)) {
    timings <- tibble::deframe(timings)
  }

  if (is.numeric(timings) && !is.null(timings)) {
    timings <- as.list(timings)
  }

  testthat::expect_is(timings, "list")

  # create output structure
  model %>% extend_with(
    "dynwrap::with_timings",
    timings = timings
  )
}

#' Test whether an object is a model and has timings information
#'
#' @param object The object to be tested.
#'
#' @export
is_wrapper_with_timings <- function(object) {
  is_data_wrapper(object) && "dynwrap::with_timings" %in% class(object)
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
  timings[[name]] <- as.numeric(Sys.time())
  timings
}
