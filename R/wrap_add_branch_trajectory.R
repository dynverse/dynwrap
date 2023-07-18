#' Construct a trajectory given its branch network and the pseudotime of the cells on one of the branches.
#'
#' The branch network is converted to a milestone network by giving each branch a start and end milestone. If two branches are connected in the branch network, the end milestone of branch 1 and start milestone of branch 2 will be merged.
#'
#' The resulting trajectory will always be directed.
#'
#' @inheritParams common_param
#' @param branch_network The network between branches, a dataframe with a *from* and *to* branch identifier
#' @param branches The length and directedness of the branches, a dataframe with the branch identifier (*branch_id*), the length of the branch (*length*) and whether it is *directed*
#' @param branch_progressions Specifies the progression of a cell along a transition in the branch network. A dataframe containing the *cell_id*, *branch_id* and its progression along the edge (*percentage*, between 0 and 1)
#' @param ... extra information to be stored in the trajectory
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
#' dataset <- wrap_data(cell_ids = letters)
#'
#' branch_network <- tibble::tibble(from = c("A", "A"), to = c("B", "C"))
#' branch_network
#' branches <- tibble::tibble(branch_id = c("A", "B", "C"), length = 1, directed = TRUE)
#' branches
#' branch_progressions <- tibble::tibble(
#'   cell_id = dataset$cell_ids,
#'   branch_id = sample(branches$branch_id, length(dataset$cell_ids), replace = TRUE),
#'   percentage = runif(length(dataset$cell_ids))
#' )
#' branch_progressions
#'
#' trajectory <- add_branch_trajectory(
#'   dataset,
#'   branch_network,
#'   branches,
#'   branch_progressions
#' )
#'
#' # for plotting the result, install dynplot
#' #- dynplot::plot_graph(trajectory)
add_branch_trajectory <- function(
  dataset,
  branch_network,
  branches,
  branch_progressions,
  ...
) {
  # check whether object is a data wrapper
  assert_that(is_data_wrapper(dataset))
  cell_ids <- dataset$cell_ids

  branch_ids <- branches$branch_id

  # check branch ids, branch network network and branches
  assert_that(is.character(branch_ids))
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
  dataset %>%
    add_trajectory(
      milestone_network = milestone_network,
      progressions = progressions
    )
}

# Check given trajectory input ----------------------------------------
check_branch_network <- function(branch_ids, branch_network) {
  assert_that(is.data.frame(branch_network))
  assert_that(ncol(branch_network) == 2)
  assert_that(setequal(colnames(branch_network), c("from", "to")))

  branch_network <- branch_network %>% select(from, to)

  assert_that(is.character(branch_network$from))
  assert_that(is.character(branch_network$to))
  assert_that(branch_network$from %all_in% branch_ids)
  assert_that(branch_network$to %all_in% branch_ids)
  assert_that(!any(duplicated(branch_network %>% select(from, to))))

  branch_network
}

check_branches <- function(branch_ids, branches) {
  assert_that(is.data.frame(branches))
  assert_that(ncol(branches) == 3)
  assert_that(setequal(colnames(branches), c("branch_id", "length", "directed")))

  branches <- branches %>% select(branch_id, length, directed)

  assert_that(is.character(branches$branch_id))
  assert_that(is.numeric(branches$length))
  assert_that(is.logical(branches$directed))
  assert_that(branches$branch_id %all_in% branch_ids)

  assert_that(!any(duplicated(branches %>% select(branch_id))))
  assert_that(!any(is.na(branches$length)))
  assert_that(!any(is.na(branches$directed)))

  branches
}

check_branch_progressions <- function(cell_ids, branch_ids, branch_progressions) {
  assert_that(is.data.frame(branch_progressions))
  assert_that(ncol(branch_progressions) == 3)
  assert_that(setequal(colnames(branch_progressions), c("cell_id", "branch_id", "percentage")))

  branch_progressions <- branch_progressions %>% select(cell_id, branch_id, percentage)

  assert_that(is.character(branch_progressions$cell_id))
  assert_that(is.character(branch_progressions$branch_id))
  assert_that(is.numeric(branch_progressions$percentage))

  assert_that(branch_progressions$cell_id %all_in% cell_ids)
  assert_that(branch_progressions$branch_id %all_in% branch_ids)

  branch_progressions
}
