#' Simplify an igraph network such that consecutive linear edges are removed
#'
#' - Nodes with degree 2 (or indegree 1 and outdegree 1) are removed: A -> B -> C becomes A -> C
#' - Cycles contain at least 3 nodes, ie. A -> B -> A becomes A -> B -> C -> A
#' - Loops are converted to a cycle, unless `allow_self_loops = TRUE`
#' - Duplicated edges are removed, unless `allow_duplcated_edges = FALSE`
#'
#' @param gr An igraph object, see [igraph::graph()]
#' @param allow_duplicated_edges Whether or not to allow duplicated edges between nodes.
#' @param allow_self_loops Whether or not to allow self loops.
#' @param force_keep Nodes that will not be removed under any condition
#' @param edge_points Points that are on edges
#'
#' @return An igraph object, or a list with an igraph object and a data frame with edge points
#'
#' @importFrom igraph V E are_adjacent is_directed degree graph_from_data_frame distances %--% %->%
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
  allow_self_loops = TRUE,
  force_keep = NULL,
  edge_points = NULL
) {
  if (allow_duplicated_edges && !is.null(edge_points)) {
    stop("allow_duplicated_edges cannot be TRUE when edge_points is not NULL")
  }

  # in the case of undirected networks, igraph can flip the edges, but this flip is not done in the edge_points
  # make sure all from -> to are present in edge points in the same directions as the graph
  if (!is.null(edge_points)) {
    edge_points <- bind_rows(
      anti_join(edge_points, igraph::as_data_frame(gr), c("from", "to")) %>% rename(from = to, to = from) %>% mutate(percentage = 1 - percentage),
      semi_join(edge_points, igraph::as_data_frame(gr), c("from", "to"))
    )
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
        edges_bothdir <- bind_rows(edge, edge %>% select(from = to, to = from)) %>% unique()
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
          tibble(from = i, to = i_prev, weight = simplify_get_edge(subgr, i, i_prev)$weight)
        )

        while (to_process[[i]]) {
          to_process[[i]] <- FALSE
          tmp <- i
          i <- simplify_get_next(neighs, i, is_directed, left = TRUE, prev = i_prev)
          i_prev <- tmp
          left_path[[length(left_path) + 1]] <- tibble(from = i, to = i_prev, weight = simplify_get_edge(subgr, i, i_prev)$weight)
        }

        # search for out end
        j <- simplify_get_j(neighs, v_rem, is_directed)
        j_prev <- v_rem

        right_path <- list(
          tibble(from = j_prev, to = j, weight = simplify_get_edge(subgr, j_prev, j)$weight)
        )

        while (to_process[[j]]) {
          to_process[[j]] <- FALSE
          tmp <- j
          j <- simplify_get_next(neighs, j, is_directed, left = FALSE, prev = j_prev)
          j_prev <- tmp
          right_path[[length(right_path) + 1]] <- tibble(from = j_prev, to = j, weight = simplify_get_edge(subgr, j_prev, j)$weight)
        }

        left_path <- bind_rows(rev(left_path)) %>%
          mutate_at(c("from", "to"), ~ igraph::V(subgr)$name[.])
        right_path <- bind_rows(right_path) %>%
          mutate_at(c("from", "to"), ~ igraph::V(subgr)$name[.])

        if (i == j && !allow_self_loops) {
          path <- bind_rows(left_path, right_path)

          if (nrow(path) == 3 || (nrow(path) == 2 && allow_duplicated_edges)) {
            # don't remove any edges,
            keep_v[path$from] <- TRUE
          } else if (nrow(path) == 2) {
            # add an empty edge between the two paths

            # rename v_rem
            nam <- igraph::V(subgr)[[v_rem]]$name
            naml <- paste0(nam, "_L")
            namr <- paste0(nam, "_R")
            igraph::V(subgr)[[v_rem]]$name <- naml

            # add new milestone
            sub_edge_points <- sub_edge_points %>% mutate_at(c("from", "to"), ~ ifelse(. == nam, namr, .))
            subgr <- subgr %>% igraph::add_vertices(1, attr = list(name = namr))
            v_add <- igraph::V(subgr)[[namr]]

            # remove edge between v_rem and j
            subgr <- subgr %>%
              igraph::add.edges(
                c(v_rem, v_add, v_add, j), attr = list(weight = c(0, simplify_get_edge(subgr, v_rem, j)$weight), directed = is_directed)
              ) %>%
              {igraph::delete.edges(., simplify_get_edge(., v_rem, j))}
            keep_v[nam] <- TRUE
          } else if (nrow(path) > 3) {
            # remove nodes, except for i_prev and j_prev
            nami <- igraph::V(subgr)[[i]]$name
            rem_path <- path %>% filter(from != nami & to != nami)

            rplcd <- simplify_replace_edges(subgr, sub_edge_points, i_prev, j_prev, rem_path, is_directed)
            subgr <- rplcd$subgr

            sub_edge_points <- rplcd$sub_edge_points
            keep_v[c(i_prev, j_prev)] <- TRUE
          }
        } else if (length(igraph::E(subgr)[i %--% j]) != 0 && !allow_duplicated_edges) {
          # if adding an edge between i and j would cause duplicates and this is not allowed
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
          rplcd <- simplify_replace_edges(subgr, sub_edge_points, i, j, path = bind_rows(left_path, right_path), is_directed)
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

  if (is.null(edge_points)) {
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

simplify_get_edge_points_on_path <- function(sub_edge_points, path) {
  rev_path <- path %>% select(from = to, to = from)

  sepaj <- sub_edge_points %>%
    anti_join(path, by = c("from", "to"))
  toflip <- sepaj %>%
    inner_join(rev_path, by = c("from", "to"))

  on_path <- bind_rows(
    sub_edge_points,
    toflip %>% rename(from = to, to = from) %>% mutate(percentage = 1 - percentage)
  ) %>%
    inner_join(path, by = c("from", "to"))

  not_on_path <-
    sepaj %>%
    anti_join(rev_path, by = c("from", "to"))

  lst(on_path, not_on_path)
}

simplify_replace_edges <- function(subgr, sub_edge_points, i, j, path, is_directed) {
  swap <- !is_directed && i > j

  if (swap) {
    # make sure i is larger than j, because otherwise igraph is going to add an edge j -> i instead if undirected
    # this will mess up the edge_points, as these will still add an edge between i -> j
    i_orig <- i
    i <- j
    j <- i_orig
  }

  path_len <- sum(path$weight)
  subgr <- subgr %>% igraph::add.edges(
    c(i, j), attr = list(weight = path_len, directed = is_directed)
  )

  if (!is.null(sub_edge_points)) {
    path <- path %>% mutate(cs = cumsum(weight) - weight)

    out <- simplify_get_edge_points_on_path(sub_edge_points, path)

    processed_edge_points <-
      out$on_path %>%
      mutate(from = igraph::V(subgr)$name[[i]], to = igraph::V(subgr)$name[[j]]) %>%
      mutate(percentage = case_when(path_len == 0 ~ 0.5, TRUE ~ (cs + percentage * weight) / path_len)) %>%
      select(id, from, to, percentage)

    if (swap) {
      processed_edge_points <- processed_edge_points %>% mutate(percentage = 1 - percentage)
    }

    sub_edge_points <- bind_rows(out$not_on_path, processed_edge_points)
  }

  lst(subgr, sub_edge_points)
}

simplify_get_edge <- function(subgr, i, j) {
  if (igraph::is_directed(subgr)) {
    igraph::E(subgr)[i %->% j]
  } else {
    igraph::E(subgr)[i %--% j]
  }
}
