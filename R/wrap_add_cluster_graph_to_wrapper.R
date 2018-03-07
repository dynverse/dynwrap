#' Add a cluster projection trajectory to a data wrapper
#'
#' This function will generate the milestone_network and progressions.
#'
#' @param data_wrapper A data wrapper to extend upon.
#' @param milestone_ids The names of the milestones.
#' @param milestone_network A network of milestones.
#' @param milestone_assignment_cells A milestone assignment of the cells.
#' @param ... extra information to be stored in the wrapper.
#'
#' @export
#'
#' @importFrom testthat expect_is expect_true expect_equal expect_length
#' @importFrom pdist pdist
add_cluster_graph_to_wrapper <- function(
  data_wrapper,
  milestone_ids,
  milestone_network,
  milestone_assignment_cells,
  ...
) {
  # check data wrapper
  testthat::expect_is(data_wrapper, "dynutils::data_wrapper")
  cell_ids <- data_wrapper$cell_ids

  # check dimred_cells
  testthat::expect_equal(cell_ids, rownames(dimred_cells))

  # check milestone_network
  milestone_ids <- rownames(dimred_milestones)
  check_milestone_network(milestone_ids, milestone_network)

  # put cells on edges
  both_directions <- bind_rows(
    milestone_network %>% select(from, to) %>% mutate(label = from, percentage = 0),
    milestone_network %>% select(from, to) %>% mutate(label = to, percentage = 1)
  )
  progressions <- data_frame(
    cell_id = cell_ids,
    label = milestone_assignment_cells[cell_id]
  ) %>%
    left_join(both_directions, by = "label") %>%
    group_by(cell_id) %>%
    arrange(desc(percentage)) %>%
    slice(1) %>%
    ungroup() %>%
    select(-label)

  # return output
  add_trajectory_to_wrapper(
    data_wrapper = data_wrapper,
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    divergence_regions = NULL,
    progressions = progressions,
    milestone_assignment_cells = milestone_assignment_cells,
    ...
  )
}

