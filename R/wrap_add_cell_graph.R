#' Constructs a trajectory using a graph between cells, by mapping cells onto a set of backbone cells.
#'
#' The cells that are part of the backbone will form the trajectory. All other cells are moved towards the nearest cell that is part of the backbone.
#'
#' @inherit add_trajectory return
#' @inheritParams common_param
#' @param cell_graph The edges between cells, a dataframe containing the *from* and *to* cells, the *length, and whether this edge is *directed*
#' @param to_keep Whether a cells is part of the backbone. May be a character vector with the identifiers of the backbone cells, or a named boolean vector whether a cell is from the backbone
#' @param milestone_prefix A prefix to add to the id of the cell ids when they are used as milestones, in order to avoid any naming conflicts,
#' @param ... extra information to be stored in the wrapper.
#'
#' @export
#'
#' @inherit add_trajectory return
#'
#' @keywords create_trajectory
#'
#' @return A trajectory object
#' 
#' @examples
#' library(dplyr)
#' dataset <- wrap_data(cell_ids = letters)
#'
#' backbone_cell_graph <- tibble::tibble(
#'   from = letters[1:10],
#'   to = letters[2:11],
#'   length = 1,
#'   directed = TRUE
#' )
#' leaves_cell_graph <- tibble::tibble(
#'   from = letters[12:26],
#'   to = sample(letters[1:11], 15, replace = TRUE),
#'   length = 1,
#'   directed = TRUE
#' )
#' cell_graph <- bind_rows(backbone_cell_graph, leaves_cell_graph)
#' cell_graph
#' to_keep <- letters[1:11]
#' to_keep
#'
#' trajectory <- add_cell_graph(dataset, cell_graph, to_keep)
#'
#' # for plotting the result, install dynplot
#' #- dynplot::plot_graph(trajectory)
add_cell_graph <- function(
  dataset,
  cell_graph,
  to_keep,
  milestone_prefix = "milestone_",
  ...
) {
  requireNamespace("igraph")

  # check data wrapper
  assert_that(is_data_wrapper(dataset))

  # optionally add length and directed if not specified
  if (!"length" %in% colnames(cell_graph)) {
    cell_graph$length <- 1
  }
  if (!"directed" %in% colnames(cell_graph)) {
    cell_graph$directed <- FALSE
  }

  # check to_keep
  if (is.character(to_keep)) {
    cell_ids <- unique(c(cell_graph$from, cell_graph$to))
    to_keep <- (cell_ids %in% to_keep) %>% set_names(cell_ids)
  } else {
    cell_ids <- names(to_keep)
  }
  assert_that(
    is.logical(to_keep),
    all(cell_ids %in% dataset$cell_ids),
    all.equal(sort(unique(c(cell_graph$from, cell_graph$to))), sort(names(to_keep)))
  )

  # check cell_graph
  check_milestone_network(cell_ids, cell_graph)

  # check is_directed
  is_directed <- any(cell_graph$directed)

  # make igraph object
  ids <- names(to_keep)
  gr <- igraph::graph_from_data_frame(cell_graph %>% rename(weight = length), directed = is_directed, vertices = ids)

  # STEP 1: for each cell, find closest milestone
  v_keeps <- names(to_keep)[to_keep]
  dists <- igraph::distances(gr, to = v_keeps)
  closest_trajpoint <- v_keeps[apply(dists, 1, which.min)]

  # STEP 2: simplify backbone
  gr <- gr %>%
    igraph::induced.subgraph(v_keeps)

  milestone_ids <- igraph::V(gr)$name

  # STEP 3: Calculate progressions of cell_ids
  # determine which nodes were on each path
  milestone_network_proto <-
    igraph::as_data_frame(gr) %>%
    as_tibble() %>%
    rowwise() %>%
    mutate(
      path = igraph::shortest_paths(gr, from, to, mode = "out")$vpath %>% map(names)
    ) %>%
    ungroup()

  # for each node, find an edge which contains the node and
  # calculate its progression along that edge
  progressions <-
    milestone_network_proto %>%
    rowwise() %>%
    do(with(., tibble(from, to, weight, node = path))) %>%
    ungroup %>%
    group_by(node) %>%
    slice(1) %>%
    mutate(
      percentage = ifelse(weight == 0, 0, igraph::distances(gr, from, node) / weight)
    ) %>%
    ungroup() %>%
    right_join(
      tibble(cell_id = ids, node = closest_trajpoint),
      by = "node"
    ) %>%
    select(cell_id, from, to, percentage)

  # create output
  milestone_network <- milestone_network_proto %>%
    select(from, to, length = weight) %>%
    mutate(directed = is_directed)

  # rename milestones so the milestones don't have the
  # same names as the nodes
  renamefun <- function(x) {
    paste0(milestone_prefix, x) %>%
      set_names(names(x))
  }

  milestone_network <- milestone_network %>%
    mutate_at(c("from", "to"), renamefun)
  milestone_ids <- milestone_ids %>%
    renamefun
  progressions <- progressions %>%
    mutate_at(c("from", "to"), renamefun)

  # return output
  add_trajectory(
    dataset = dataset,
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    divergence_regions = NULL,
    progressions = progressions,
    ...
  ) %>%
    simplify_trajectory()
}
