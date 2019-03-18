#' Add count and normalised expression values to a model
#'
#' @param model The model to which timings will be added
#' @param timings A list of timings.
#'
#' @keywords adapt_trajectory
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

#' @inheritParams add_timings
#' @rdname add_timings
#'
#' @export
is_wrapper_with_timings <- function(model) {
  is_data_wrapper(model) && "dynwrap::with_timings" %in% class(model)
}

#' Helper function for storing timings information.
#'
#' @param timings The timings list of previous checkpoints.
#' @param name The name of the timings checkpoint.
#'
#' @rdname add_timings
#'
#' @export
add_timing_checkpoint <- function(timings, name) {
  if (is.null(timings)) {
    timings <- list()
  }
  timings[[name]] <- as.numeric(Sys.time())
  timings
}
