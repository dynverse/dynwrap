#' A data wrapper for datasets and trajectories
#'
#' @param id A unique identifier for the data. If `NULL`, a random string will be generated.
#' @param cell_ids The identifiers of the cells.
#' @param cell_info Optional meta-information pertaining the cells.
#' @param feature_ids The identifiers of the features.
#' @param feature_info Optional meta-information pertaining the features.
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
wrap_data <- function(
  id = NULL,
  cell_ids,
  cell_info = NULL,
  feature_ids = NULL,
  feature_info = NULL,
  ...
) {

  # process id
  if (is.null(id)) {
    id <- dynutils::random_time_string("data_wrapper")
  }
  assert_that(
    is.character(id),
    length(id) == 1
  )

  # process cell ids
  if (is_tibble(cell_ids) && ncol(cell_ids) == 1 && "cell_ids" %in% names(cell_ids)) {
    cell_ids <- cell_ids$cell_ids
  }
  assert_that(
    is.character(cell_ids),
    !any(duplicated(cell_ids))
  )

  # process cell info
  if (is.null(cell_info)) {
    cell_info <- tibble(cell_id = cell_ids)
  }
  assert_that(
    is.data.frame(cell_info),
    all.equal(cell_info$cell_id, cell_ids)
  )

  # process feature ids
  if (!is.null(feature_ids)) {
    if (is_tibble(feature_ids) && ncol(feature_ids) == 1 && "feature_ids" %in% names(feature_ids)) {
      feature_ids <- feature_ids$feature_ids
    }
    assert_that(
      is.character(feature_ids),
      !any(duplicated(feature_ids))
    )

    # process feature info
    if (is.null(feature_info)) {
      feature_info <- tibble(feature_id = feature_ids)
    }
    assert_that(
      is.data.frame(cell_info),
      all.equal(cell_info$cell_id, cell_ids)
    )
  }



  list() %>% extend_with(
    "dynwrap::data_wrapper",
    id = id,
    cell_ids = cell_ids,
    cell_info = cell_info,
    feature_ids = feature_ids,
    feature_info = feature_info,
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
