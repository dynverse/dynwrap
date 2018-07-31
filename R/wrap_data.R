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
  id = "",
  cell_ids,
  cell_info = NULL,
  ...
) {
  testthat::expect_is(id, "character")
  testthat::expect_length(id, 1)

  if (is_tibble(cell_ids) && ncol(cell_ids) == 1 && "cell_ids" %in% names(cell_ids)) {cell_ids <- cell_ids$cell_ids}
  testthat::expect_is(cell_ids, "character")

  testthat::expect_false(any(duplicated(cell_ids)))

  if (!is.null(cell_info)) {
    testthat::expect_is(cell_info, "data.frame")
    testthat::expect_equal(cell_info$cell_id, cell_ids)
  }

  list() %>% extend_with(
    "dynwrap::data_wrapper",
    id = id,
    cell_ids = cell_ids,
    cell_info = cell_info,
    ...
  )
}

#' Test whether an object is a data_wrapper
#'
#' @param object The object to be tested.
#'
#' @export
is_data_wrapper <- function(object) {
  "dynwrap::data_wrapper" %in% class(object)
}
