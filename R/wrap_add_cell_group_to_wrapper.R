#' Add a cell grouping to a data wrapper
#'
#' @param data_wrapper A data wrapper to extend upon.
#' @param cell_group A grouping of the cells.
#' @param ... Extra information to be stored in the wrapper.
#'
#' @export
#'
#' @importFrom testthat expect_equal expect_is expect_true
add_cell_group_to_wrapper <- function(
  data_wrapper,
  cell_group,
  ...
) {
  testthat::expect_true(is_data_wrapper(data_wrapper))

  cell_ids <- data_wrapper$cell_ids

  testthat::expect_named(cell_group)
  testthat::expect_is(cell_group, c("character", "factor"))
  testthat::expect_true(all(names(cell_group) %in% cell_ids))

  # create output structure
  out <- c(
    data_wrapper,
    list(
      cell_group = cell_group,
      ...
    ))
  class(out) <- c("dynwrap::with_cell_group", class(data_wrapper))
  out
}

#' Test whether an object is a data_wrapper and has cell_group data
#'
#' @param object The object to be tested.
#'
#' @export
is_wrapper_with_cell_group <- function(object) {
  is_data_wrapper(object) && "dynwrap::with_cell_group" %in% class(object)
}
