#' Multifurcating trajectory with end state probabilities
#'
#' Constructs a multifurcating trajectory using the pseudotime values of each cell and their end state probabilities.
#' If pseudotime values are not given, will use pseudotime already present in the model.
#'
#' This function will generate the milestone_network and progressions.
#'
#' @param model The model to which a multifurcating trajectory will be added.
#' @param pseudotime A named vector of pseudo times.
#' @param end_state_probabilities A dataframe containing cell_id (character) and additional numeric columns containing the probability for every end milestone. If the data_frame contains only a cell_id column, the data will be processed using `add_linear_trajectory`
#' @param do_scale_minmax Whether or not to scale the pseudotime between 0 and 1.
#'   Otherwise, will assume the values are already within that range.
#' @param ... Extras to be added to the model
#'
#' @return The trajectory model
#'
#' @export
#'
#' @importFrom testthat expect_true
add_end_state_probabilities <- function(
  model,
  end_state_probabilities,
  pseudotime = NULL,
  do_scale_minmax = TRUE,
  ...
) {
  # check data wrapper
  expect_true(is_data_wrapper(model))

  # process pseudotime
  if (!is.null(pseudotime)) {
    pseudotime <- process_pseudotime(model, pseudotime)
    # scale pseudotime
    if (do_scale_minmax) {
      pseudotime <- scale_minmax(pseudotime)
    } else {
      expect_true(all(0 <= pseudotime & pseudotime <= 1))
    }
    model <- model %>% add_pseudotime(pseudotime)
  } else {
    expect_true("pseudotime" %in% names(model))
    pseudotime <- model$pseudotime
  }

  # process end_state_probabilities
  expect_true("cell_id" %in% colnames(end_state_probabilities))

  if (ncol(end_state_probabilities) == 1) {
    # process as linear of no end_state_probabilities are provided
    model %>%
      add_linear_trajectory(
        pseudotime = pseudotime,
        directed = TRUE,
        do_scale_minmax = do_scale_minmax
      )
  } else {
    # construct milestone ids and milestone network
    start_milestone_id <- "milestone_start"
    end_milestone_ids <- colnames(end_state_probabilities)[colnames(end_state_probabilities) != "cell_id"]
    milestone_ids <- c(start_milestone_id, end_milestone_ids)

    milestone_network <- data_frame(
      from = start_milestone_id,
      to = end_milestone_ids,
      length = 1,
      directed = TRUE
    )

    # one divergence region
    divergence_regions <- data_frame(
      milestone_id = milestone_ids,
      divergence_id = "D",
      is_start = milestone_ids == start_milestone_id
    )

    # construct progressions
    progressions <- end_state_probabilities %>%
      gather("to", "percentage", -cell_id) %>%
      mutate(from = start_milestone_id) %>%
      group_by(cell_id) %>%
      mutate(percentage = percentage / sum(percentage)) %>%  # scale percentage so that sum = 1
      ungroup()

    # return output
    add_trajectory(
      model = model,
      milestone_ids = milestone_ids,
      milestone_network = milestone_network,
      divergence_regions = divergence_regions,
      progressions = progressions,
      end_state_probabilities = end_state_probabilities,
      ...
    )
  }
}
