#' Add a linear trajectory to a data wrapper
#'
#' This function will generate the milestone_network and progressions.
#'
#' @param data_wrapper A data wrapper to extend upon.
#' @param pseudotime A named vector of pseudo times.
#' @param directed Whether or not the directionality of the pseudotime is predicted.
#' @param do_scale_minmax Whether or not to scale the pseudotime between 0 and 1.
#'   Otherwise, will assume the values are already within that range.
#' @param ... extra information to be stored in the wrapper.
#'
#' @export
#'
#' @importFrom testthat expect_is expect_true expect_named
add_linear_trajectory <- function(
  data_wrapper,
  pseudotime,
  directed = FALSE,
  do_scale_minmax = TRUE,
  ...
) {
  # check data wrapper
  testthat::expect_true(is_data_wrapper(data_wrapper))

  # check names of pseudotime
  cell_ids <- data_wrapper$cell_ids
  testthat::expect_is(pseudotime, "numeric")
  testthat::expect_named(pseudotime)
  testthat::expect_true(all(names(pseudotime) %in% cell_ids))

  # scale pseudotime
  if (do_scale_minmax) {
    pseudotime <- scale_minmax(pseudotime)
  } else {
    testthat::expect_true(all(0 <= pseudotime & pseudotime <= 1))
  }

  # construct milestones
  milestone_ids <- c("milestone_start", "milestone_end")

  # construct milestone_network
  milestone_network <- data_frame(
    from = milestone_ids[[1]],
    to = milestone_ids[[2]],
    length = 1,
    directed = directed
  )

  # construct progressions
  progressions <- data_frame(
    cell_id = names(pseudotime),
    from = milestone_ids[[1]],
    to = milestone_ids[[2]],
    percentage = pseudotime
  )

  # return output
  add_trajectory(
    data_wrapper = data_wrapper,
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    divergence_regions = NULL,
    progressions = progressions,
    pseudotime = pseudotime,
    ...
  )
}




# Process linear from file -------------------------------------
process_linear <- function(model, dir_output) {
  pseudotime <- read_pseudotime(dir_output)
  model %>% add_linear_trajectory(pseudotime)
}

output_processors <- output_processors %>% add_row(
  id="linear",
  processor=list(process_linear),
  required_files=list(c("pseudotime.csv")),
  optional_files=list(c()),
  required_output=list(c()),
  description="Creates a linear trajectory from pseudotime",
  creates_trajectory=TRUE
)

