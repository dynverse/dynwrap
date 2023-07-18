#' Constructs a linear trajectory using pseudotime values
#'
#' @inheritParams common_param
#' @param pseudotime A named vector of pseudo times.
#' @param directed Whether the trajectory will be directed.
#' @param do_scale_minmax Whether or not to scale the pseudotime between 0 and 1.
#'   Otherwise, will assume the values are already within that range.
#' @param ... extra information to be stored in the trajectory
#'
#' @keywords create_trajectory
#'
#' @inherit add_trajectory return
#'
#' @return A trajectory object
#'
#' @export
#'
#' @examples
#' library(tibble)
#' dataset <- wrap_data(cell_ids = letters)
#'
#' pseudotime <- tibble(
#'   cell_id = dataset$cell_ids,
#'   pseudotime = runif(length(dataset$cell_ids))
#' )
#'
#' trajectory <- add_linear_trajectory(dataset, pseudotime)
add_linear_trajectory <- function(
  dataset,
  pseudotime,
  directed = FALSE,
  do_scale_minmax = TRUE,
  ...
) {
  # check data wrapper
  assert_that(is_data_wrapper(dataset))

  pseudotime <- process_pseudotime(dataset, pseudotime)

  # scale pseudotime
  if (do_scale_minmax) {
    pseudotime <- scale_minmax(pseudotime)
  } else {
    assert_that(all(0 <= pseudotime & pseudotime <= 1))
  }

  # construct milestones
  milestone_ids <- c("milestone_begin", "milestone_end")

  # construct milestone_network
  milestone_network <- tibble(
    from = milestone_ids[[1]],
    to = milestone_ids[[2]],
    length = 1,
    directed = directed
  )

  # construct progressions
  progressions <- tibble(
    cell_id = names(pseudotime),
    from = milestone_ids[[1]],
    to = milestone_ids[[2]],
    percentage = pseudotime
  )

  # return output
  add_trajectory(
    dataset = dataset,
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    divergence_regions = NULL,
    progressions = progressions,
    pseudotime = pseudotime,
    ...
  )
}
