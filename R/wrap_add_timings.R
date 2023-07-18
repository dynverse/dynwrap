#' Add timings to a trajectory
#'
#' @inheritParams common_param
#' @param timings A list of timings.
#'
#' @keywords adapt_trajectory
#'
#' @return A dynwrap object with the timings added.
#'
#' @examples
#' trajectory <- example_trajectory
#' trajectory <- add_timings(
#'   trajectory,
#'   list(start = 0, end = 1)
#' )
#'
#' @export
add_timings <- function(
  trajectory,
  timings
) {
  assert_that(is_data_wrapper(trajectory))

  if (is.data.frame(timings)) {
    timings <- tibble::deframe(timings)
  }

  if (is.numeric(timings) && !is.null(timings)) {
    timings <- as.list(timings)
  }

  assert_that(is.list(timings))

  # create output structure
  trajectory %>% extend_with(
    "dynwrap::with_timings",
    timings = timings
  )
}

#' @inheritParams add_timings
#' @rdname add_timings
#'
#' @export
is_wrapper_with_timings <- function(trajectory) {
  is_data_wrapper(trajectory) && "dynwrap::with_timings" %in% class(trajectory)
}

#' Helper function for storing timings information.
#'
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
