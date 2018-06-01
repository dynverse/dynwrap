#' Add a cluster projection trajectory to a data wrapper
#'
#' This function will generate the milestone_network and progressions.
#'
#' @param data_wrapper A data wrapper to extend upon. Needs to have a cell grouping created by [add_grouping()].
#' @param milestone_network A network of milestones.
#' @param ... extra information to be stored in the wrapper.
#'
#' @export
#'
#' @importFrom testthat expect_is expect_true expect_equal
#' @importFrom pdist pdist
add_cluster_graph <- function(
  data_wrapper,
  milestone_network,
  ...
) {
  # check data wrapper
  testthat::expect_true(is_data_wrapper(data_wrapper))
  testthat::expect_true(is_wrapper_with_grouping(data_wrapper))

  # get milestone_ids
  milestone_ids <- data_wrapper$group_ids

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
    cell_id = names(data_wrapper$grouping),
    label = data_wrapper$grouping
  ) %>%
    left_join(both_directions, by = "label") %>%
    group_by(cell_id) %>%
    arrange(desc(percentage)) %>%
    slice(1) %>%
    ungroup() %>%
    select(-label)

  # return output
  add_trajectory(
    data_wrapper = data_wrapper,
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    divergence_regions = NULL,
    progressions = progressions,
    ...
  )
}






# Process cluster graph from file ---------------

process_cluster_graph <- function(model, dir_output) {
  if(!is_wrapper_with_grouping(model)) {
    model <- process_grouping(model, dir_output)
  }

  milestone_network <- read_milestone_network(dir_output)
  model %>% add_cluster_graph(milestone_network)
}

output_processors <- output_processors %>% add_row(
  id="cluster_graph",
  processor=list(process_cluster_graph),
  required_files=list(c("milestone_network.csv")),
  optional_files=list(c()),
  required_output=list(c("grouping")),
  description="Creates a trajectory by using a cell grouping and a given network between these cell groups",
  creates_trajectory=TRUE
)
