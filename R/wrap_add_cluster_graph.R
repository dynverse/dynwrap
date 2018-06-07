#' Constructs a trajectory using a cell grouping and a network between groups. Will use an existing grouping if it is present in the model.
#'
#' This function will generate the milestone_network and progressions.
#'
#' @param model The model to which a cluster graph will be added. Needs to have a cell grouping created by [add_grouping()].
#' @param milestone_network A network of milestones.
#' @inheritParams add_grouping
#' @param ... extra information to be stored in the wrapper.
#'
#' @export
#'
#' @return The trajectory model
#'
#' @importFrom testthat expect_is expect_true expect_equal
#' @importFrom pdist pdist
add_cluster_graph <- function(
  model,
  milestone_network,
  grouping = NULL,
  ...
) {
  # check data wrapper
  testthat::expect_true(is_data_wrapper(model))

  # get grouping from model if not provided
  if (is.null(grouping)) {
    testthat::expect_true(is_wrapper_with_grouping(model))
  } else {
    model <- model %>% add_grouping(grouping)
  }
  grouping <- get_grouping(model)

  milestone_ids <- unique(c(milestone_network$to, milestone_network$from))

  # check milestone network
  check_milestone_network(milestone_ids, milestone_network)

  # put cells on edges.
  # prefer to put a cell at the end of a transition, but put it at the start
  # if there is no other option.
  both_directions <- bind_rows(
    milestone_network %>% select(from, to) %>% mutate(label = from, percentage = 0),
    milestone_network %>% select(from, to) %>% mutate(label = to, percentage = 1)
  )
  progressions <- data_frame(
    cell_id = names(grouping),
    label = grouping
  ) %>%
    left_join(both_directions, by = "label") %>%
    group_by(cell_id) %>%
    arrange(desc(percentage)) %>%
    slice(1) %>%
    ungroup() %>%
    select(-label)

  # return output
  add_trajectory(
    model = model,
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    divergence_regions = NULL,
    progressions = progressions,
    ...
  )
}
