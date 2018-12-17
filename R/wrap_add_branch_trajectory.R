#' Define a trajectory model given its branch network and the pseudotime of the cells on one of the branches
#'
#' @param model The model to which the trajectory will be added.
#' @param branch_network The network between branches
#'   Type: Data frame(from = charactor, to = character)
#' @param branches The length and directedness of the branches
#'   Type: Data frame(branch_id = character, length = numeric, directed = logical)
#' @param branch_progressions Specifies the progression of a cell along a transition in the branch network.
#'   Type: Data frame(cell_id = character, branch_id = character, percentage = numeric).
#' @param ... extra information to be stored in the model
#'
#' @return The trajectory model
#'
#' @export
#'
#' @importFrom testthat expect_is expect_equal expect_true expect_false
add_branch_trajectory <- function(
  model,
  branch_network,
  branches,
  branch_progressions,
  ...
) {
  # check whether object is a data wrapper
  testthat::expect_true(is_data_wrapper(model))
  cell_ids <- model$cell_ids

  branch_ids <- branches$branch_id

  # check branch ids, branch network network and branches
  testthat::expect_is(branch_ids, "character")
  branch_network <- check_branch_network(branch_ids, branch_network)
  branches <- check_branches(branch_ids, branches)

  # check branch progressions
  branch_progressions <- check_branch_progressions(cell_ids, branch_ids, branch_progressions)

  # create the milestone network
  milestone_network <- bind_rows(
    tibble(
      from = paste0(branch_ids, "_from"),
      to = paste0(branch_ids, "_to"),
      branch_id = branch_ids
    )
  )

  milestone_mapper_network <- tibble(
    from = paste0(branch_ids, "_from"),
    to = paste0(branch_ids, "_from")
  ) %>% bind_rows(
    tibble(
      from = paste0(branch_network$from, "_to"),
      to = paste0(branch_network$to, "_from")
    )
  ) %>% bind_rows(
    tibble(
      from = paste0(branch_ids, "_to"),
      to = paste0(branch_ids, "_to")
    )
  )
  mapper <- milestone_mapper_network %>% igraph::graph_from_data_frame() %>% igraph::components() %>% .$membership
  milestone_network$from <- as.character(mapper[milestone_network$from])
  milestone_network$to <- as.character(mapper[milestone_network$to])

  # merge branches info with milestone network
  milestone_network <- milestone_network %>%
    left_join(branches, "branch_id")

  # add extra milestones between loops, ie. A -> A becomes A -> A-0a -> A-0b -> A
  new_edge_length <- sum(milestone_network$length)/nrow(milestone_network)/100
  for (branch_id in milestone_network %>% filter(from == to) %>% pull(branch_id)) {
    milestone_id <- milestone_network %>% filter(branch_id == !!branch_id) %>% pull(from)
    new_milestone_ids <- paste0(milestone_id, "-", branch_id, c("a", "b"))
    milestone_network <- milestone_network %>%
      mutate(to = ifelse(branch_id == !!branch_id, new_milestone_ids[1], to)) %>%
      bind_rows(
        tibble(
          from = new_milestone_ids,
          to = c(new_milestone_ids[2], milestone_id),
          branch_id = "whatever",
          directed = TRUE,
          length = new_edge_length
        )
      )
  }

  # create progressions
  progressions <- branch_progressions %>%
    left_join(milestone_network, "branch_id") %>%
    select(cell_id, from, to, percentage)

  milestone_network <- milestone_network %>%
    select(from, to, length, directed)

  # create trajectory
  model %>%
    add_trajectory(
      milestone_network = milestone_network,
      progressions = progressions
    )
}

# Check given trajectory input ----------------------------------------
#' @importFrom testthat expect_is expect_equal expect_true
check_branch_network <- function(branch_ids, branch_network) {
  testthat::expect_is(branch_network, "data.frame")
  testthat::expect_equal(ncol(branch_network), 2)
  testthat::expect_setequal(colnames(branch_network), c("from", "to"))
  branch_network <- branch_network %>% select(from, to)
  testthat::expect_equal(sapply(branch_network, class), c(from = "character", to = "character"))
  testthat::expect_true(all(branch_network$from %in% branch_ids))
  testthat::expect_true(all(branch_network$to %in% branch_ids))
  testthat::expect_false(any(duplicated(branch_network %>% select(from, to))))

  branch_network
}

check_branches <- function(branch_ids, branches) {
  testthat::expect_is(branches, "data.frame")
  testthat::expect_equal(ncol(branches), 3)
  testthat::expect_setequal(colnames(branches), c("branch_id", "length", "directed"))
  branches <- branches %>% select(branch_id, length, directed)
  testthat::expect_equal(sapply(branches, class), c(branch_id = "character", length = "numeric", directed = "logical"))
  testthat::expect_true(all(branches$branch_id %in% branch_ids))
  testthat::expect_false(any(duplicated(branches %>% select(branch_id))))
  testthat::expect_false(any(is.na(branches$length)))
  testthat::expect_false(any(is.na(branches$directed)))

  branches
}

#' @importFrom testthat expect_is expect_equal expect_true
check_branch_progressions <- function(cell_ids, branch_ids, branch_progressions) {
  testthat::expect_is(branch_progressions, "data.frame")
  testthat::expect_equal(ncol(branch_progressions), 3)
  testthat::expect_setequal(colnames(branch_progressions), c("cell_id", "branch_id", "percentage"))
  branch_progressions <- branch_progressions %>% select(cell_id, branch_id, percentage)
  testthat::expect_equal(sapply(branch_progressions, class), c(cell_id = "character", branch_id = "character", percentage = "numeric"))
  testthat::expect_true(all(branch_progressions$cell_id %in% cell_ids))
  testthat::expect_true(all(branch_progressions$branch_id %in% branch_ids))

  branch_progressions
}
