#' Simplify an igraph network such that consecutive linear edges are removed
#'
#' @param gr an igraph object
#' @param allow_duplicated_edges Whether or not to allow duplicated edges between nodes.
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
simplify_igraph_network <- function(gr, allow_duplicated_edges = TRUE) {
  # add weight attribute if not already present
  if (!"weight" %in% names(igraph::edge.attributes(gr))) {
    igraph::E(gr)$weight <- 1
  }
  # add directed attribute if not already present
  if (!"directed" %in% names(igraph::edge.attributes(gr))) {
    igraph::E(gr)$directed <- igraph::is_directed(gr)
  }

  # process each component of the graph separatly
  graphs <- igraph::decompose.graph(gr)
  simplified_graphs <- map(graphs, function(gri) {
    is_loop <- igraph::V(gri) %>%
      map_lgl(~ igraph::are_adjacent(gri, ., .))

    is_directed <- igraph::is_directed(gri)

    if (is_directed) {
      degr_in <- igraph::degree(gri, mode = "in")
      degr_out <- igraph::degree(gri, mode = "out")

      keep_v <- degr_in != 1 | degr_out != 1 | is_loop

      if (sum(keep_v) == 0) {
        # if keep is character(0), gri is a simple cycle
        igraph::graph_from_data_frame(
          data_frame(
            from = names(igraph::V(gri))[[1]],
            to = from,
            weight = sum(igraph::E(gri)$weight),
            directed = is_directed
          ),
          directed = is_directed
        )
      } else {
        num_vs <- igraph::V(gri) %>% length

        # igraph::neighbors(
        neighs_in <- seq_len(num_vs) %>% map(~igraph::neighbors(gri, ., mode = "in") %>% as.integer)
        neighs_out <- seq_len(num_vs) %>% map(~igraph::neighbors(gri, ., mode = "out") %>% as.integer)
        to_process <- !keep_v

        for (v_rem in seq_len(num_vs)) {
          if (to_process[[v_rem]]) {
            to_process[[v_rem]] <- FALSE

            # search for in end
            i <- neighs_in[[v_rem]]
            i_prev <- v_rem
            dis_rem_l <- igraph::E(gri)[i %->% i_prev]$weight
            while (to_process[[i]]) {
              to_process[[i]] <- FALSE
              i_prev <- i
              i <- neighs_in[[i]]
              dis_rem_l <- dis_rem_l + igraph::E(gri)[i %->% i_prev]$weight
            }

            # search for out end
            j <- neighs_out[[v_rem]]
            j_prev <- v_rem
            dis_rem_r <- igraph::E(gri)[j_prev %->% j]$weight
            while (to_process[[j]]) {
              to_process[[j]] <- FALSE
              j_prev <- j
              j <- neighs_out[[j]]
              dis_rem_r <- dis_rem_r + igraph::E(gri)[j_prev %->% j]$weight
            }

            # if adding an edge between i and j would cause duplicates and this is not allowed
            if (length(igraph::E(gri)[i %->% j]) != 0 && !allow_duplicated_edges) {
              if (i_prev != v_rem) {
                gri <- gri %>% igraph::add.edges(
                  c(i, v_rem), attr = list(weight = dis_rem_l, directed = is_directed)
                )
              }
              if (j_prev != v_rem) {
                gri <- gri %>% igraph::add.edges(
                  c(v_rem, j), attr = list(weight = dis_rem_r, directed = is_directed)
                )
              }
              keep_v[v_rem] <- TRUE
            } else {
              gri <- gri %>% igraph::add.edges(
                c(i, j),
                attr = list(
                  weight = dis_rem_l + dis_rem_r,
                  directed = is_directed
                )
              )
            }

          }
        }

        gri %>% igraph::delete.vertices(which(!keep_v))
      }
    } else {
      degr <- igraph::degree(gri)
      keep_v <- degr != 2 | is_loop

      if (sum(keep_v) == 0) {
        # if keep is empty, gri is a simple cycle
        igraph::graph_from_data_frame(
          data_frame(
            from = names(igraph::V(gri))[[1]],
            to = from,
            weight = sum(igraph::E(gri)$weight),
            directed = is_directed
          ),
          directed = is_directed
        )
      } else {
        num_vs <- igraph::V(gri) %>% length

        neighs <- seq_len(num_vs) %>% map(~igraph::neighbors(gri, .) %>% as.integer)
        to_process <- !keep_v

        edges_to_add <- list()

        for (v_rem in seq_len(num_vs)) {
          if (to_process[[v_rem]]) {
            to_process[[v_rem]] <- FALSE

            # search for in end
            i <- neighs[[v_rem]][[1]]
            i_prev <- v_rem

            dis_rem_l <- igraph::E(gri)[i %--% i_prev]$weight

            while (to_process[[i]]) {
              to_process[[i]] <- FALSE
              i_new <- setdiff(neighs[[i]], i_prev)
              if (length(i_new) > 0) {
                i_prev <- i
                i <- i_new
                dis_rem_l <- dis_rem_l + igraph::E(gri)[i %--% i_prev]$weight
              }
            }

            # search for out end
            j <- neighs[[v_rem]][[2]]
            j_prev <- v_rem

            dis_rem_r <- igraph::E(gri)[j %--% j_prev]$weight
            while (to_process[[j]]) {
              to_process[[j]] <- FALSE
              j_new <- setdiff(neighs[[j]], j_prev)
              if (length(j_new) > 0) {
                j_prev <- j
                j <- j_new
                dis_rem_r <- dis_rem_r + igraph::E(gri)[j %--% j_prev]$weight
              }
            }

            # if adding an edge between i and j would cause duplicates and this is not allowed
            if (length(igraph::E(gri)[i %--% j]) != 0 && !allow_duplicated_edges) {
              if (i_prev != v_rem) {
                gri <- gri %>% igraph::add.edges(
                  c(i, v_rem), attr = list(weight = dis_rem_l, directed = is_directed)
                )
              }
              if (j_prev != v_rem) {
                gri <- gri %>% igraph::add.edges(
                  c(v_rem, j), attr = list(weight = dis_rem_r, directed = is_directed)
                )
              }
              keep_v[v_rem] <- TRUE
            } else {
              gri <- gri %>% igraph::add.edges(
                c(i, j),
                attr = list(
                  weight = dis_rem_l + dis_rem_r,
                  directed = is_directed
                )
              )
            }
          }
        }

        gri %>% igraph::delete.vertices(which(!keep_v))
      }
    }
  })

  # combine the graphs
  # we don't use igraph::union here as it renames the edge attributes
  map_df(simplified_graphs, igraph::as_data_frame) %>%
    bind_rows() %>%
    igraph::graph_from_data_frame(directed = igraph::is_directed(gr))
}
