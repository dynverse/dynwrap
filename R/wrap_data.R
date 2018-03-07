#' A data wrapper for datasets and trajectories
#'
#' @param id A unique identifier for the data
#' @param cell_ids The ids of the cells.
#' @param cell_info Optional meta-information pertaining the cells.
#' @param ... Extra information to be stored in the wrapper.
#'
#' @export
#'
#' @importFrom testthat expect_is expect_length expect_equal
wrap_data <- function(
  id,
  cell_ids,
  cell_info = NULL,
  ...
) {
  testthat::expect_is(id, "character")
  testthat::expect_length(id, 1)

  testthat::expect_is(cell_ids, "character")

  if (!is.null(cell_info)) {
    testthat::expect_is(cell_info, "data.frame")
    testthat::expect_equal(cell_info$cell_id, cell_ids)
  }

  out <- list(
    id = id,
    cell_ids = cell_ids,
    cell_info = cell_info,
    ...
  )
  class(out) <- c("dynutils::data_wrapper", class(out))
  out
}

#' Test whether an object is a data_wrapper
#'
#' @param object The object to be tested.
#'
#' @export
is_data_wrapper <- function(object) {
  "dynutils::data_wrapper" %in% class(object)
}
