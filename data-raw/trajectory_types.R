library(tidyverse)
library(colorspace)

################################
## Construct trajectory_types ##
################################

lighten <- function(color, factor = 1.4){
  purrr::map_chr(color, function(color) {
    col <- col2rgb(color)
    col <- do.call(rgb2hsv, as.list(col))
    col[1] <- col[1] * 360
    col[2] <- 0.3
    col[3] <- 0.9
    colorspace::hex(do.call(colorspace::HSV, as.list(col)))
  })
}

trajectory_types <- tribble(
  ~id, ~colour,
  "cycle", "#003d76",
  "linear", "#0278dd",
  "bifurcation", "#3ad1d1",
  "convergence", "#1f8888",
  "multifurcation", "#7fbe00",
  "binary_tree", "#00b009",
  "tree", "#e0ab00",
  "acyclic_graph", "#ff8821",
  "graph", "#ff4237",
  "disconnected_graph", "#ca0565"
) %>% mutate(
  background_colour = lighten(colour, 0.3)
)

###################################
## Construct trajectory_type_dag ##
###################################

trajectory_type_dag <- tribble(
  ~to, ~from, ~prop_changes,
  "bifurcation", "linear", "n_branch_nodes == 0",
  "bifurcation", "convergence", "switch_directedness",
  "convergence", "bifurcation", "switch_directedness",
  "convergence", "linear", "n_branch_nodes == 0",
  "multifurcation", "bifurcation", "max_degree == 3",
  "binary_tree", "bifurcation", "num_branch_nodes == 1",
  "tree", "multifurcation", "num_branch_nodes == 1",
  "tree", "binary_tree", "max_degree == 3",
  "acyclic_graph", "tree", "max_indegree == 1",
  "graph", "acyclic_graph", "num_cycles == 0",
  "disconnected_graph", "graph", "num_components == 1",
  "graph", "cycle", c("num_branch_nodes == 0", "num_cycles == 1")
) %>%
  igraph::graph_from_data_frame(vertices = trajectory_types) %>%
  tidygraph::as_tbl_graph()

trajectory_type_ancestors <- trajectory_type_dag %>%
  igraph::ego(99999999, mode = "in") %>%
  map(names) %>%
  set_names(names(igraph::V(trajectory_type_dag)))

trajectory_types$ancestors <- trajectory_type_ancestors[trajectory_types$id]

######################
## Save all objects ##
######################

devtools::use_data(trajectory_types, trajectory_type_dag, overwrite = TRUE)



