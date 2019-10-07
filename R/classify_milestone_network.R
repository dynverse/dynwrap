#' Classify a milestone network
#'
#' @param milestone_network A milestone network
#'
#' @return A list containing
#' - *network_type*: The network type (also known as the trajectory_type). See `dynwrap::trajectory_types` for an overview.
#' - *directed*: Whether the trajectory is directed
#' - *properties*: Different properties of the trajectory, including:
#'   - *is_directed*: Whether the trajectory is directed
#'   - *max_degree*: The maximal degree
#'   - *num_branch_nodes*: The number of branching nodes
#'   - *num_outer_nodes*: Number of leaf (outer) nodes
#'   - *is_self_loop*: Whether it contains self-loops
#'   - *has_cycles*: Whether it has cycles
#'   - *num_components*: The number of independent components
#'
#' @export
#'
#' @examples
#' milestone_network <- tibble::tibble(
#'   from = c("A", "B", "C"),
#'   to = c("B", "C", "A"),
#'   length = 1,
#'   directed = TRUE
#' )
#' classification <- classify_milestone_network(milestone_network)
#' classification$network_type
#' classification$directed
#'
#' milestone_network <- tibble::tibble(
#'   from = c("A", "B", "B", "C", "C"),
#'   to = c("B", "C", "D", "E", "F"),
#'   length = 2,
#'   directed = FALSE
#' )
#' classification <- classify_milestone_network(milestone_network)
#' classification$network_type
#' classification$directed
#' classification$props
#'
#' @importFrom igraph graph_from_data_frame
#'
#' @seealso `dynwrap::trajectory_types`
classify_milestone_network <- function(milestone_network) {
  is_directed <- any(milestone_network$directed)

  gr <- milestone_network %>%
    mutate(weight = length) %>%
    igraph::graph_from_data_frame(directed = is_directed) %>%
    simplify_igraph_network()

  props <- determine_milenet_props(gr)

  network_type <- determine_network_type(props)

  lst(network_type, directed = props$is_directed, properties = props)
}

determine_network_type <- function(props) {
  with(props, {
    if (is_directed) {
      if (num_components > 1) {
        "disconnected_graph"
      } else {
        if (!has_cycles) {
          if (num_branch_nodes == 0) {
            "linear"
          } else if (num_branch_nodes == 1) {

            if (num_convergences == 0) {
              if (max_degree <= 3 && max_degree_out < 3) {
                "bifurcation"
              } else {
                "multifurcation"
              }
            } else if (num_divergences == 0) {
              "convergence"
            } else {
              "acyclic_graph"
            }

          } else {
            if (num_convergences == 0) {
              "tree"
            } else {
              "acyclic_graph"
            }
          }
        } else {
          if (num_branch_nodes == 0) {
            "cycle"
          } else
            "graph"
        }
      }

    } else {
      if (num_components > 1) {
        "disconnected_graph"
      } else {
        if (!has_cycles) {
          if (num_branch_nodes == 0) {
            "linear"
          } else if (num_branch_nodes == 1) {
            if (max_degree == 3) {
              "bifurcation"
            } else {
              "multifurcation"
            }
          } else {
            "tree"
          }
        } else {
          if (num_branch_nodes == 0) {
            "cycle"
          } else {
            "graph"
          }
        }
      }
    }
  })
}


determine_milenet_props <- function(gr) {
  requireNamespace("igraph")

  # is the graph directed
  is_directed <- igraph::is_directed(gr)

  # does it contain self-loops?
  is_self_loop <- sapply(igraph::V(gr), function(n) igraph::are_adjacent(gr, n, n))

  # number of components
  num_components <- igraph::components(gr)$no

  if (is_directed) {
    # degree
    degr_in <- igraph::degree(gr, mode = "in")
    degr_out <- igraph::degree(gr, mode = "out")
    degr_tot <- degr_in + degr_out

    max_degree_in <- max(degr_in)
    max_degree_out <- max(degr_out)

    # begin/end/branch/outer
    is_begin <- degr_in == is_self_loop & degr_out != 0
    is_end <- degr_in != 0 & degr_out == 0
    is_branch <- degr_out > 1 | degr_in > 1
    is_outer <- is_begin | is_end
    num_begin_nodes <- sum(is_begin)
    num_end_nodes <- sum(is_end)
    num_branch_nodes <- sum(is_branch)
    num_outer_nodes <- sum(is_outer)

    # divergences & convergences
    num_divergences <- sum(degr_out > 1)
    num_convergences <- sum(degr_in > 1)
  } else {
    # degree
    degr_tot <- igraph::degree(gr)

    # branch/outer
    is_outer <- degr_tot == 1 + is_self_loop
    is_branch <- !is_outer
    num_outer_nodes <- sum(is_outer)
    num_branch_nodes <- sum(is_branch)
  }

  max_degree <- max(degr_tot)

  # does the graph have cycles?
  has_cycles <- any(is_self_loop) || has_cycle_function(gr)

  # output data
  out <- lst(
    is_directed,
    max_degree,
    degr_tot,
    is_branch,
    is_outer,
    num_branch_nodes,
    num_outer_nodes,
    is_self_loop,
    has_cycles,
    num_components
  )

  if (is_directed) {
    c(out, lst(
      degr_in,
      degr_out,
      max_degree_in,
      max_degree_out,
      num_divergences,
      num_convergences,
      is_begin,
      is_end,
      num_begin_nodes,
      num_end_nodes
    ))
  } else {
    out
  }
}

has_cycle_function <- function(gr) {
  if (igraph::is_directed(gr)) {
    for (from in igraph::V(gr)) {
      for (to in igraph::V(gr)) {
        if (from != to) {
          one <- igraph::distances(gr, from, to, mode = "out")
          two <- igraph::distances(gr, to, from, mode = "out")
          if (is.finite(one) && is.finite(two)) {
            return(TRUE)
          }
        }
      }
    }
    return(FALSE)
  } else {
    for (from in names(igraph::V(gr))) {
      # if two nodes are connected by multiple edges, return true
      if (any(duplicated(igraph::neighbors(gr, from) %>% names))) {
        return(TRUE)
      }
      # if there are two different paths between two distinct nodes, return true
      for (to in names(igraph::V(gr))) {
        if (from != to && igraph::are_adjacent(gr, from, to) && length(igraph::E(gr)) > 1) {
          newgr <- gr - igraph::edge(paste0(from, "|", to))
          if (is.finite(igraph::distances(newgr, from, to))) {
            return(TRUE)
          }
        }
      }
    }
    return(FALSE)
  }
}



