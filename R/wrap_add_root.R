#' Root the trajectory
#'
#' Designates a milestone as root, and changes the direction of any edges so that they move away from the specified root (if `flip_edges=TRUE`, default).
#'
#' A `root_cell_id` can also be specified, and the root milestone will be determined as the milestone with the closest geodesic distance to this cell.
#'
#' @inheritParams common_param
#' @param root_cell_id The root cell id, not required if root_milestone_id is given
#' @param root_milestone_id The root milestone id, not required if root_cell_id is given
#' @param flip_edges Whether to flip edges which are going in the other direction compared to the root
#'
#' @keywords adapt_trajectory
#'
#' @return A trajectory, with a *root_milestone_id* and with adapted *milestone_network* and *progressions* based on the rooting.
#'
#' @examples
#' # add a root using a root cell
#' trajectory <- example_trajectory
#' trajectory <- add_root(
#'   trajectory,
#'   root_cell_id = sample(trajectory$cell_ids, 1)
#' )
#' trajectory$root_milestone_id
#'
#' # add a root using a root milestone id
#' trajectory <- add_root(
#'   trajectory,
#'   root_milestone_id = "milestone_end"
#' )
#' trajectory$root_milestone_id
#' trajectory$milestone_network
#'
#' @importFrom purrr map2_int
#'
#' @export
add_root <- function(
  trajectory,
  root_cell_id = trajectory$root_cell_id,
  root_milestone_id = trajectory$root_milestone_id,
  flip_edges = TRUE
) {
  if (!is.null(root_cell_id)) {
    if(!root_cell_id %in% trajectory$cell_ids) {stop("Invalid root_cell_id")}

    root_milestone_id <- trajectory$milestone_percentages %>% filter(cell_id == root_cell_id) %>% filter(percentage == max(percentage)) %>% pull(milestone_id)
  } else if (is.null(root_milestone_id)) {
    message("root cell or milestone not provided, trying first outgoing milestone_id")
    root_milestone_id <- setdiff(trajectory$milestone_network$from, trajectory$milestone_network$to) %>% first()

    if(is.na(root_milestone_id)) {
      message("Could not find outgoing milestone_id, using first milestone_id as root")
      root_milestone_id <- trajectory$milestone_network$from[[1]]
    }

    message(paste0("Using '", root_milestone_id, "' as root"))
  }

  if (flip_edges) {
    gr <- igraph::graph_from_data_frame(
      trajectory$milestone_network %>% rename(weight = length),
      directed = any(trajectory$milestone_network$directed)
    )

    # TODO: allow to add multiple roots for disconnected trajectories??

    # get milestones already downstream of the root
    ord1 <- igraph::distances(gr, v = root_milestone_id, mode = "out")[1,] %>% keep(is.finite) %>% sort() %>% names()

    # add milestones upstream of the root id
    ord2 <- igraph::distances(gr, v = root_milestone_id, mode = "all")[1,] %>% keep(is.finite) %>% sort() %>% names()
    milestone_order <- union(ord1, ord2)

    # why though? should disconnected milestones be reordered?
    # I'm putting this in comments for now.
    # # add disconnected milestones.
    # milestone_order <- intersect(milestone_order, trajectory$milestone_ids)

    # determine which edges to flip
    milestone_network_toflip <-
      trajectory$milestone_network %>%
      mutate(
        flip = match(from, milestone_order) > match(to, milestone_order)
      ) %>%
      filter(!is.na(flip), flip)

    trajectory <- flip_edges(trajectory, milestone_network_toflip)

    # order milestone network
    milestone_order <-
      trajectory$milestone_network %>%
      igraph::graph_from_data_frame() %>%
      igraph::dfs(root_milestone_id, unreachable = TRUE) %>%
      .$order %>%
      names()

    trajectory$milestone_network <-
      trajectory$milestone_network %>%
      arrange(map2_int(from, to, ~max(which(milestone_order %in% c(.x, .y)))))
  }

  trajectory$root_milestone_id <- root_milestone_id

  trajectory
}


#' Add root cell to wrapper using expression of features
#'
#' @param features_oi The feature ids which will be used to root
#' @param expression_source Source of the expression, either a string or a matrix
#'
#' @inheritParams add_root
#' @rdname add_root
#'
#' @export
add_root_using_expression <- function(trajectory, features_oi, expression_source = "expression") {
  expression <- get_expression(trajectory, expression_source)

  root_cell_id <- rownames(expression)[expression[, features_oi, drop = F] %>% Matrix::rowMeans() %>% which.max()]
  trajectory <- add_root(trajectory, root_cell_id)

  trajectory
}



#' @inheritParams add_root
#' @rdname add_root
#'
#' @export
is_rooted <- function(trajectory) {
  is.null(trajectory$root_milestone_id)
}


#' @inheritParams add_root
#' @rdname add_root
#'
#' @export
remove_root <- function(trajectory) {
  trajectory$root_milestone_id <- NULL
  trajectory
}
