#' Root trajectory
#'
#' Roots the trajectory by changing the directionality of all edges given a root cell
#'
#' @param trajectory the trajectory object
#' @param root_cell_id The root cell id, not required if root_milestone_id is given
#' @param root_milestone_id The root milestone id, not required if root_cell_id is given
#'
#' @importFrom purrr map2_int
#'
#' @export
root_trajectory <- function(trajectory, root_cell_id = NULL, root_milestone_id = trajectory$root_milestone_id) {
  if (!is.null(root_cell_id)) {
    if(!root_cell_id %in% trajectory$cell_ids) {stop("Invalid root_cell_id")}

    root_milestone_id <- trajectory$milestone_percentages %>% filter(cell_id == root_cell_id) %>% filter(percentage == max(percentage)) %>% pull(milestone_id)
  } else if (is.null(root_milestone_id)) {
    if(!is.null(trajectory$root_cell_id)) {
      root_cell_id <- trajectory$root_cell_id
      root_milestone_id <- trajectory$milestone_percentages %>% filter(cell_id == root_cell_id) %>% filter(percentage == max(percentage)) %>% pull(milestone_id)
      message("Using root_cell_id as root")
    } else {
      message("root cell or milestone not provided, trying first outgoing milestone_id")
      root_milestone_id <- setdiff(trajectory$milestone_ids, trajectory$milestone_network$to) %>% first()

      if(is.na(root_milestone_id)) {
        message("Could not find outgoing milestone_id, using first milestone_id as root")
        root_milestone_id <- trajectory$milestone_ids[[1]]
      }

      message(paste0("Using '", root_milestone_id, "' as root"))
    }
  }

  milestone_order <- igraph::graph_from_data_frame(trajectory$milestone_network) %>% igraph::ego(nodes=root_milestone_id, 999) %>% first() %>% names()

  # flip edge if from is later than to
  trajectory$milestone_network <- trajectory$milestone_network %>%
    mutate(
      flip = match(from, milestone_order) > match(to, milestone_order)
    )

  # flip milestone network & progressions
  trajectory$progressions <- trajectory$progressions %>%
    left_join(trajectory$milestone_network %>% select(from, to, flip), c("from", "to")) %>%
    mutate(
      from2 = from,
      from = ifelse(flip, to, from),
      to = ifelse(flip, from2, to),
      percentage = ifelse(flip, 1-percentage, percentage)
    ) %>%
    select(-flip, -from2)

  trajectory$milestone_network <- trajectory$milestone_network %>%
    mutate(
      from2 = from,
      from = ifelse(flip, to, from),
      to = ifelse(flip, from2, to),
      directed = TRUE
    ) %>%
    select(-flip, -from2)

  # order milestone network
  milestone_order <- trajectory$milestone_network %>%
    igraph::graph_from_data_frame() %>%
    igraph::dfs(root_milestone_id) %>%
    .$order %>%
    names()

  trajectory$milestone_network <- trajectory$milestone_network %>%
    arrange(map2_int(from, to, ~max(which(milestone_order %in% c(.x, .y)))))

  trajectory$root_milestone_id <- root_milestone_id

  trajectory
}



#' Calculate global pseudotime as distance from root
#'
#' @param trajectory The trajectory
#' @export
calculate_pseudotime <- function(trajectory) {
  if(!"root_milestone_id" %in% trajectory) {
    trajectory <- root_trajectory(trajectory)
  }

  root_cell_id <- trajectory$milestone_percentages %>%
    filter(milestone_id == trajectory$root_milestone_id) %>%
    arrange(desc(percentage)) %>%
    pull(cell_id) %>%
    first()

  if(is.na(root_cell_id)) {stop("Could not find rooting cell for pseudotime calculation")}

  pseudotime <- compute_tented_geodesic_distances(trajectory, root_cell_id)[1, ]

  pseudotime
}




#' Add root cell to wrapper using expression of features
#'
#' @param task The task
#' @param feature_ids The feature ids
#' @export
add_root_using_expression <- function(task, features_oi, expression_source = "expression") {
  expression <- get_expression(task, expression_source)

  task$root_cell_id <- rownames(expression)[expression[, features_oi, drop=F] %>% rowMeans() %>% which.max()]

  task
}
