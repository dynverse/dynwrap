#' Simplify an igraph network such that consecutive linear edges are removed
#'
#' @param gr an igraph object
#'
#' @importFrom igraph V are_adjacent is.directed degree graph_from_data_frame distances
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
simplify_igraph_network <- function(gr) {
  is_loop <- igraph::V(gr) %>%
    map_lgl(~ igraph::are_adjacent(gr, ., .))

  # add weight attribute if not already present
  if (!"weight" %in% names(igraph::edge.attributes(gr))) {
    igraph::E(gr)$weight <- 1
  }

  if (igraph::is.directed(gr)) {
    degr_in <- igraph::degree(gr, mode = "in")
    degr_out <- igraph::degree(gr, mode = "out")
    keep_v <- degr_in != 1 | degr_out != 1 | is_loop

    if (sum(keep_v) == 0) {
      # if keep is character(0), gr is a simple cycle
      igraph::graph_from_data_frame(
        data_frame(
          from = names(igraph::V(gr))[[1]],
          to = from,
          weight = sum(igraph::E(gr)$weight)
        ),
        directed = igraph::is.directed(gr)
      )
    } else {
      num_vs <- igraph::V(gr) %>% length

      # igraph::neighbors(
      neighs_in <- seq_len(num_vs) %>% map(~igraph::neighbors(gr, ., mode = "in") %>% as.integer)
      neighs_out <- seq_len(num_vs) %>% map(~igraph::neighbors(gr, ., mode = "out") %>% as.integer)
      to_process <- !keep_v

      edges_to_add <- list()

      for (v_rem in seq_len(num_vs)) {
        if (to_process[[v_rem]]) {
          to_process[[v_rem]] <- FALSE

          # search for in end
          i <- neighs_in[[v_rem]]
          while (to_process[[i]]) {
            to_process[[i]] <- FALSE
            i <- neighs_in[[i]]
          }

          # search for out end
          j <- neighs_out[[v_rem]]
          while (to_process[[j]]) {
            to_process[[j]] <- FALSE
            j <- neighs_out[[j]]
          }

          edges_to_add[[length(edges_to_add)+1]] <- list(from = i, to = j)
        }
      }

      weights_to_add <- sapply(edges_to_add, function(e) igraph::distances(gr, e[[1]], e[[2]])[1,1])
      weights_to_add[weights_to_add == 0] <- sum(igraph::E(gr)$weight)

      gr2 <- gr
      if (length(edges_to_add) > 0) {
        gr2 <- gr2 %>% igraph::add.edges(unlist(edges_to_add), attr = list(weight = weights_to_add, directed=TRUE))
      }
      gr2 %>% igraph::delete.vertices(which(!keep_v))
    }
  } else {
    degr <- igraph::degree(gr)
    keep_v <- degr != 2 | is_loop

    if (sum(keep_v) == 0) {
      # if keep is character(0), gr is a simple cycle
      igraph::graph_from_data_frame(
        data_frame(
          from = names(igraph::V(gr))[[1]],
          to = from,
          weight = sum(igraph::E(gr)$weight)
        ),
        directed = igraph::is.directed(gr)
      )
    } else {
      num_vs <- igraph::V(gr) %>% length

      # igraph::neighbors(
      neighs <- seq_len(num_vs) %>% map(~igraph::neighbors(gr, .) %>% as.integer)
      to_process <- !keep_v

      edges_to_add <- list()

      for (v_rem in seq_len(num_vs)) {
        if (to_process[[v_rem]]) {
          to_process[[v_rem]] <- FALSE

          # search for in end
          i <- neighs[[v_rem]][[1]]
          i_prev <- v_rem
          while (to_process[[i]]) {
            to_process[[i]] <- FALSE
            i_new <- setdiff(neighs[[i]], i_prev)
            if (length(i_new) > 0) {
              i_prev <- i
              i <- i_new
            }
          }

          # search for out end
          j <- neighs[[v_rem]][[2]]
          j_prev <- v_rem
          while (to_process[[j]]) {
            to_process[[j]] <- FALSE
            j_new <- setdiff(neighs[[j]], j_prev)
            if (length(j_new) > 0) {
              j_prev <- j
              j <- j_new
            }
          }

          edges_to_add[[length(edges_to_add)+1]] <- list(from = i, to = j)
        }
      }

      weights_to_add <- sapply(edges_to_add, function(e) igraph::distances(gr, e[[1]], e[[2]])[1,1])
      weights_to_add[weights_to_add == 0] <- sum(igraph::E(gr)$weight)

      gr2 <- gr
      if (length(edges_to_add) > 0) {
        gr2 <- gr2 %>% igraph::add.edges(unlist(edges_to_add), attr = list(weight = weights_to_add, directed=TRUE))
      }
      gr2 %>% igraph::delete.vertices(which(!keep_v))
    }
  }


}
