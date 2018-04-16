#' Add a cell grouping to a data wrapper
#'
#' @param data_wrapper A data wrapper to extend upon.
#' @param group_ids The ids of the groupings.
#' @param cell_group A grouping of the cells.
#' @param ... Extra information to be stored in the wrapper.
#'
#' @export
#'
#' @importFrom testthat expect_equal expect_is expect_true
add_cell_group_to_wrapper <- function(
  data_wrapper,
  group_ids,
  cell_group,
  ...
) {
  # check whether object is a data wrapper
  testthat::expect_true(is_data_wrapper(data_wrapper))

  # check group ids
  testthat::expect_is(group_ids, "character")
  testthat::expect_false(any(duplicated(group_ids)))

  # check cell group
  testthat::expect_named(cell_group)
  testthat::expect_is(cell_group, "character")
  testthat::expect_true(all(names(cell_group) %in% data_wrapper$cell_ids))
  testthat::expect_true(all(cell_group %in% group_ids))

  # check milestone ids, if data contains a trajectory
  if (is_wrapper_with_trajectory(data_wrapper)) {
    testthat::expect_equal(data_wrapper$milestone_ids, group_ids)
  }

  # create output structure
  data_wrapper %>% extend_with(
    "dynwrap::with_cell_group",
    group_ids = group_ids,
    cell_group = cell_group,
    ...
  )
}

#' Test whether an object is a data_wrapper and has cell_group data
#'
#' @param object The object to be tested.
#'
#' @export
is_wrapper_with_cell_group <- function(object) {
  is_data_wrapper(object) && "dynwrap::with_cell_group" %in% class(object)
}
