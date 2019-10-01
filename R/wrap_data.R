#' A data wrapper for datasets and trajectories
#'
#' @param id A unique identifier for the data. If `NULL`, a random string will be generated.
#' @param cell_ids The identifiers of the cells.
#' @param cell_info Optional meta-information pertaining the cells.
#' @param ... Extra information to be stored in the wrapper.
#'
#' @return A list containing *id*, *cell_ids* and *cell_info* (if specified)
#'
#' @keywords create_trajectory
#'
#' @export
#'
#' @examples
#' dataset <- wrap_data(
#'   cell_ids = c("A", "B", "C")
#' )
#' dataset$cell_ids
#'
#' @importFrom testthat expect_is expect_length expect_equal
wrap_data <- function(
  id = NULL,
  cell_ids,
  cell_info = NULL,
  ...
) {
  if (is.null(id)) {
    id <- dynutils::random_time_string("data_wrapper")
  }

  testthat::expect_is(id, "character")
  testthat::expect_length(id, 1)

  if (is_tibble(cell_ids) && ncol(cell_ids) == 1 && "cell_ids" %in% names(cell_ids)) {
    cell_ids <- cell_ids$cell_ids
  }
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

#' @rdname wrap_data
#' @inheritParams common_param
#'
#' @export
is_data_wrapper <- function(dataset) {
  "dynwrap::data_wrapper" %in% class(dataset)
}
