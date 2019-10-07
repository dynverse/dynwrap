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
  ~id, ~colour, ~example_network, ~example_nodes,
  "cycle", "#003d76", tribble(~from, ~to, 1, 2, 2, 3, 3, 1), tribble(~x, ~y, 0, 0, 3, 4, 6, 0),
  "linear", "#0278dd", tribble(~from, ~to, 1, 2), tribble(~x, ~y, 0, 2, 6, 2),
  "convergence", "#1f8888", tribble(~from, ~to, 1, 3, 2, 3, 3, 4),  tribble(~x, ~y, 0, 0, 0, 4, 3, 2, 6, 2),
  "bifurcation", "#3ad1d1", tribble(~from, ~to, 1, 2, 2, 3, 2, 4),  tribble(~x, ~y, 0, 2, 3, 2, 6, 4, 6, 0),
  "multifurcation", "#7fbe00", tribble(~from, ~to, 1, 2, 2, 3, 2, 4, 2, 5),  tribble(~x, ~y, 0, 2, 3, 2, 6, 4, 6, 2, 6, 0),
  "tree", "#e0ab00", tribble(~from, ~to, 1, 2, 2, 3, 2, 4, 2, 5, 5, 6, 5, 7), tribble(~x, ~y, 0, 2, 2, 2, 4, 4, 4, 0, 4, 2, 6, 3, 6, 1),
  "acyclic_graph", "#ff8821", tribble(~from, ~to, 1, 2, 2, 3, 2, 4, 3, 5, 4, 5), tribble(~x, ~y, 0, 2, 2, 2, 4, 4, 4, 0, 6, 2),
  "graph", "#ff4237", tribble(~from, ~to, 1, 2, 2, 3, 3, 4, 2, 5, 5, 6, 5, 7, 7, 8, 8, 2), tribble(~x, ~y, 0, 2, 2, 2, 2, 0, 4, 0, 4, 2, 6, 2, 4, 4, 2, 4),
  "disconnected_graph", "#ca0565", tribble(~from, ~to, 1, 2, 2, 3, 3, 4, 4, 2, 5, 6, 6, 7, 6, 8), tribble(~x, ~y, 0, 2, 2, 2, 4, 0, 3, 4, 5, 0, 5, 2, 4, 4, 6, 4)
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
  "tree", "multifurcation", "num_branch_nodes == 1",
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


#' @examples
#' # compile first, before running this
#' df <- trajectory_types %>%
#'   mutate(
#'     xmin = 0,
#'     xmax = 1,
#'     ymin = row_number() + .1,
#'     ymax = ymin + .8
#'   )
#' dynbenchmark::plot_trajectory_types(
#'   plot = ggplot() + geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), df, fill = "lightgray") + geom_text(aes(xmin, ymax, label = id), df, hjust = 0, vjust = 0) + theme_void(),
#'   trajectory_types = df$id,
#'   xmins = df$xmin, xmaxs = df$xmax, ymins = df$ymin, ymaxs = df$ymax
#' )

######################
## Save all objects ##
######################

devtools::use_data(trajectory_types, trajectory_type_dag, overwrite = TRUE)



