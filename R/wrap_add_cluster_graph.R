#' Constructs a trajectory using a cell grouping and a network between groups. Will use an existing grouping if it is present in the model.
#'
#' This function will generate the milestone_network and progressions.
#'
#' @param model The model to which a cluster graph will be added. Needs to have a cell grouping created by [add_grouping()].
#' @param milestone_network A network of milestones.
#' @param explicit_splits Whether to make splits specific by adding a starting node. For example: A->B, A->C becomes A->X, X->B, X->C
#' @inheritParams add_grouping
#' @param ... extra information to be stored in the wrapper.
#'
#' @return The trajectory model
#'
#' @keywords create_trajectory
#'
#' @importFrom testthat expect_is expect_true expect_equal
#' @importFrom pdist pdist
#'
#' @export
add_cluster_graph <- function(
  model,
  milestone_network,
  grouping = NULL,
  explicit_splits = FALSE,
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
