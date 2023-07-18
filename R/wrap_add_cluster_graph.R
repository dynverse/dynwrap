#' Constructs a trajectory using a cell grouping and a network between groups. Will use an existing grouping if it is present in the dataset.
#'
#' A trajectory in this form will rarely be useful, given that cells are only placed at the milestones themselves, but not on the edges between milestones. A better alternative might be to project the cells using a dimensionality reduction, see [add_dimred_projection()].
#'
#' @inheritParams common_param
#' @inheritParams add_grouping
#' @param milestone_network A network of milestones.
#' @param explicit_splits Whether to make splits specific by adding a starting node. For example: A->B, A->C becomes A->X, X->B, X->C
#' @param ... extra information to be stored in the wrapper.
#'
#' @inherit add_trajectory return
#'
#' @keywords create_trajectory
#' 
#' @return A trajectory object
#'
#' @export
#'
#' @examples
#' library(tibble)
#' dataset <- wrap_data(cell_ids = letters)
#'
#' milestone_network <- tibble::tibble(
#'   from = c("A", "B", "B"),
#'   to = c("B", "C", "D"),
#'   directed = TRUE,
#'   length = 1
#' )
#' milestone_network
#' grouping <- sample(c("A", "B", "C", "D"), length(dataset$cell_ids), replace = TRUE)
#' grouping
#' trajectory <- add_cluster_graph(dataset, milestone_network, grouping)
#'
#' # for plotting the result, install dynplot
#' #- dynplot::plot_graph(trajectory)
add_cluster_graph <- function(
  dataset,
  milestone_network,
  grouping = NULL,
  explicit_splits = FALSE,
  ...
) {
  # check data wrapper
  assert_that(is_data_wrapper(dataset))

  # get grouping from dataset if not provided
  if (is.null(grouping)) {
    assert_that(is_wrapper_with_grouping(dataset))
  } else {
    dataset <- dataset %>% add_grouping(grouping)
  }
  grouping <- get_grouping(dataset)
  grouping <- grouping[!is.na(grouping)]

  milestone_ids <- unique(c(milestone_network$to, milestone_network$from))

  # check milestone network
  check_milestone_network(milestone_ids, milestone_network)

  # add explicit splits if requested
  if (explicit_splits) {
    milestone_network <- cluster_graph_add_explicit_splits(milestone_network)
    milestone_ids <- unique(c(milestone_network$to, milestone_network$from))
  }

  # put cells on edges.
  # prefer to put a cell at the end of a transition, but put it at the start
  # if there is no other option.
  both_directions <- bind_rows(
    milestone_network %>% select(from, to) %>% mutate(label = from, percentage = 0),
    milestone_network %>% select(from, to) %>% mutate(label = to, percentage = 1)
  )
  progressions <- tibble(
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
    dataset = dataset,
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    divergence_regions = NULL,
    progressions = progressions,
    ...
  )
}




cluster_graph_add_explicit_splits <- function(milestone_network) {
  # add extra splits
  milestone_ids_implicit_split <- table(milestone_network$from) %>% keep(~.>=2) %>% names() %>% discard(~. %in% milestone_network$to)

  if (length(milestone_ids_implicit_split) > 0) {
    milestone_ids_explicit_split <- paste0("split_", milestone_ids_implicit_split) %>% set_names(milestone_ids_implicit_split)
    milestone_network <- milestone_network %>%
      mutate(
        from = ifelse(from %in% names(milestone_ids_explicit_split), milestone_ids_explicit_split[from], from)
      ) %>%
      bind_rows(
        tibble(
          from = names(milestone_ids_explicit_split),
          to = milestone_ids_explicit_split,
          length = 0,
          directed = TRUE
        )
      )
  }

  # add extra convergences
  milestone_ids_implicit_convergence <- table(milestone_network$to) %>% keep(~.>=2) %>% names() %>% discard(~. %in% milestone_network$from)

  if (length(milestone_ids_implicit_convergence) > 0) {
    milestone_ids_explicit_convergence <- paste0("convergence_", milestone_ids_implicit_convergence) %>% set_names(milestone_ids_implicit_convergence)
    milestone_network <- milestone_network %>%
      mutate(
        to = ifelse(to %in% names(milestone_ids_explicit_convergence), milestone_ids_explicit_convergence[to], to)
      ) %>%
      bind_rows(
        tibble(
          from = milestone_ids_explicit_convergence,
          to = names(milestone_ids_explicit_convergence),
          length = 0,
          directed = TRUE
        )
      )
  }


  milestone_network
}
