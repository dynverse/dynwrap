#' Simplify an igraph network such that consecutive linear edges are removed
#'
#' @param gr an igraph object
#' @param allow_duplicated_edges Whether or not to allow duplicated edges between nodes.
#' @param force_keep Nodes that will not be removed under any condition
#' @param edge_points Points that are on edges
#'
#' @importFrom igraph V E are_adjacent is_directed degree graph_from_data_frame distances
#'
#' @export
#'
#' @examples
#' net <- data.frame(
#'   from = 1:2,
#'   to = 2:3,
#'   length = 1,
#'   directed = TRUE,
#'   stringsAsFactors = F
#' )
#' gr <- igraph::graph_from_data_frame(net)
#' simplify_igraph_network(gr)
#'
#' net <- data.frame(
#'   from = c(1, 2, 3, 1),
#'    to = c(2, 3, 1, 4),
#'     length = 1,
#'     directed = TRUE,
#'     stringsAsFactors = F
#' )
#' gr <- igraph::graph_from_data_frame(net)
#' simplify_igraph_network(gr)
#'
#' net <- data.frame(
#'   from = c(1, 2, 3, 4),
#'    to = c(2, 3, 1, 5),
#'     length = 1,
#'     directed = TRUE,
#'     stringsAsFactors = F
#' )
#' gr <- igraph::graph_from_data_frame(net)
#' simplify_igraph_network(gr)
simplify_igraph_network <- function(
  gr,
  allow_duplicated_edges = TRUE,
  force_keep = NULL,
  edge_points = NULL
) {
  if (allow_duplicated_edges && !is.null(edge_points)) {
    stop("allow_duplicated_edges is cannot be TRUE when edge_points is not NULL")
  }

  # to make sure indexing is not confused with names
  igraph::V(gr)$name <- paste0("#M#", igraph::V(gr)$name)
  if (!is.null(force_keep)) {
    force_keep <- paste0("#M#", force_keep)
  }
  if (!is.null(edge_points)) {
    edge_points <- edge_points %>% mutate_at(c("from", "to"), ~ paste0("#M#", .))
  }

  # add weight attribute if not already present
  if (!"weight" %in% names(igraph::edge.attributes(gr))) {
    igraph::E(gr)$weight <- 1
  }

  # add directed attribute if not already present
  if (!"directed" %in% names(igraph::edge.attributes(gr))) {
    igraph::E(gr)$directed <- igraph::is_directed(gr)
  }

  is_directed <- igraph::is_directed(gr)

  # process each component of the graph separatly
  graphs <- igraph::decompose.graph(gr)

  simplified_graphs <- map(graphs, function(subgr) {
    keep_v <- simplify_determine_nodes_to_keep(subgr = subgr, is_directed = is_directed, force_keep = force_keep)
    sub_edge_points <-
      if (!is.null(edge_points)) {
        edge <- igraph::as_data_frame(subgr) %>% select(from, to)
        edges_bothdir <- bind_rows(edge, edge %>% select(from = to, to = from))
        inner_join(edges_bothdir, edge_points, by = c("from", "to"))
      } else {
        NULL
      }

    # subgr is a cycle; at least one node should be kept
    if (sum(keep_v) == 0) {
      keep_v[[1]] <- TRUE
    }

    num_vs <- igraph::V(subgr) %>% length
    neighs <- simplify_get_neighbours(subgr, is_directed)
    to_process <- !keep_v

    for (v_rem in seq_len(num_vs)) {
      if (to_process[[v_rem]]) {
        to_process[[v_rem]] <- FALSE

        # search for in end
        i <- simplify_get_i(neighs, v_rem, is_directed)
        i_prev <- v_rem

        left_path <- list(
          data_frame(from = i, to = i_prev, weight = igraph::E(subgr)[i %--% i_prev]$weight)
        )

        while (to_process[[i]]) {
          to_process[[i]] <- FALSE
          tmp <- i
          i <- simplify_get_next(neighs, i, is_directed, left = TRUE, prev = i_prev)
          i_prev <- tmp
          left_path[[length(left_path) + 1]] <- data_frame(from = i, to = i_prev, weight = igraph::E(subgr)[i %--% i_prev]$weight)
        }

        # search for out end
        j <- simplify_get_j(neighs, v_rem, is_directed)
        j_prev <- v_rem

        right_path <- list(
          data_frame(from = j_prev, to = j, weight = igraph::E(subgr)[j %--% j_prev]$weight)
        )

        while (to_process[[j]]) {
          to_process[[j]] <- FALSE
          tmp <- j
          j <- simplify_get_next(neighs, j, is_directed, left = FALSE, prev = j_prev)
          j_prev <- tmp
          right_path[[length(right_path) + 1]] <- data_frame(from = j_prev, to = j, weight = igraph::E(subgr)[j %--% j_prev]$weight)
        }

        left_path <- bind_rows(left_path) %>%
          mutate_at(c("from", "to"), ~ igraph::V(subgr)$name[.])
        right_path <- bind_rows(right_path) %>%
          mutate_at(c("from", "to"), ~ igraph::V(subgr)$name[.])

        # if adding an edge between i and j would cause duplicates and this is not allowed
        if (length(igraph::E(subgr)[i %--% j]) != 0 && !allow_duplicated_edges) {
          if (i_prev != v_rem) {
            rplcd <- simplify_replace_edges(subgr, sub_edge_points, i, v_rem, left_path, is_directed)
            subgr <- rplcd$subgr
            sub_edge_points <- rplcd$sub_edge_points
          }
          if (j_prev != v_rem) {
            rplcd <- simplify_replace_edges(subgr, sub_edge_points, v_rem, j, right_path, is_directed)
            subgr <- rplcd$subgr
            sub_edge_points <- rplcd$sub_edge_points
          }
          keep_v[v_rem] <- TRUE
        } else {
          rplcd <- simplify_replace_edges(subgr, sub_edge_points, i, j, bind_rows(left_path, right_path), is_directed)
          subgr <- rplcd$subgr
          sub_edge_points <- rplcd$sub_edge_points
        }
      }
    }

    subgr <- subgr %>% igraph::delete.vertices(which(!keep_v))
    lst(subgr, sub_edge_points)
  })

  # combine the graphs
  # we don't use igraph::union here as it renames the edge attributes
  subgrs <- map(simplified_graphs, ~ .$subgr)
  edge_df <- map_df(subgrs, igraph::as_data_frame, what = "edges")
  node_df <- map_df(subgrs, igraph::as_data_frame, what = "vertices")
  seps <- map_df(simplified_graphs, ~ .$sub_edge_points)

  outgr <- igraph::graph_from_data_frame(edge_df, is_directed, node_df)
  igraph::V(outgr)$name <- gsub("^#M#", "", igraph::V(outgr)$name)

  if (nrow(seps) == 0) {
    outgr
  } else {
    seps <- seps %>% mutate_at(c("from", "to"), ~ gsub("^#M#", "", .))
    lst(gr = outgr, edge_points = seps)
  }
}

simplify_determine_nodes_to_keep <- function(subgr, is_directed, force_keep) {
  name_check <- igraph::V(subgr)$name %in% force_keep

  loop_check <- igraph::V(subgr) %>%
    map_lgl(~ igraph::are_adjacent(subgr, ., .))

  degr_check <-
    if (is_directed) {
      igraph::degree(subgr, mode = "in") != 1 | igraph::degree(subgr, mode = "out") != 1
    } else {
      igraph::degree(subgr) != 2
    }

  name_check | loop_check | degr_check
}

simplify_get_neighbours <- function(subgr, is_directed) {
  num_vs <- igraph::V(subgr) %>% length

  if (is_directed) {
    neighs_in <- seq_len(num_vs) %>% map(~igraph::neighbors(subgr, ., mode = "in") %>% as.integer)
    neighs_out <- seq_len(num_vs) %>% map(~igraph::neighbors(subgr, ., mode = "out") %>% as.integer)
    lst(
      neighs_in,
      neighs_out
    )
  } else {
    neighs <- seq_len(num_vs) %>% map(~igraph::neighbors(subgr, .) %>% as.integer)
    lst(
      neighs
    )
  }
}

simplify_get_i <- function(neighs, v_rem, is_directed) {
  if (is_directed) {
    neighs$neighs_in[[v_rem]]
  } else {
    neighs$neighs[[v_rem]][[1]]
  }
}
simplify_get_j <- function(neighs, v_rem, is_directed) {
  if (is_directed) {
    neighs$neighs_out[[v_rem]]
  } else {
    neighs$neighs[[v_rem]][[2]]
  }
}
simplify_get_next <- function(neighs, v_rem, is_directed, left = NA, prev = NA) {
  if (is_directed) {
    if (left) {
      neighs$neighs_in[[v_rem]]
    } else {
      neighs$neighs_out[[v_rem]]
    }
  } else {
    setdiff(neighs$neighs[[v_rem]], prev)
  }
}

simplify_replace_edges <- function(subgr, sub_edge_points, i, j, path, is_directed) {
  path_len <- sum(path$weight)
  subgr <- subgr %>% igraph::add.edges(
    c(i, j), attr = list(weight = path_len, directed = is_directed)
  )

  if (!is.null(sub_edge_points)) {
    path <- path %>% mutate(cs = cumsum(weight) - weight)
    sub_edge_points <- bind_rows(
      anti_join(sub_edge_points, path, by = c("from", "to")),
      sub_edge_points %>%
        inner_join(path, by = c("from", "to")) %>%
        mutate(from = igraph::V(subgr)$name[[i]], to = igraph::V(subgr)$name[[j]]) %>%
        mutate(percentage = (cs + percentage * weight) / path_len) %>%
        select(id, from, to, percentage)
    )
  }

  lst(subgr, sub_edge_points)
}


#' @examples
#' set.seed(1)
#' traj <- dyntoy::generate_dataset(model = dyntoy::model_connected(3, 3))
#' dynplot::plot_graph(traj, label_milestones = TRUE)
#' gr <- igraph::graph_from_data_frame(traj$milestone_network, directed = TRUE, traj$milestone_ids)
#' divergence_regions <- traj$divergence_regions
#' progressions <- traj$progressions
#'
