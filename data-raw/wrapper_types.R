library(tidyverse)

wrapper_types <- tribble(
  ~id,                  ~long_name,              ~short_name, ~colour,
  "direct",             "Direct",                "Direct",    "#ff3c2f",
  "linear",             "Linear pseudotime",     "Linear",    "#0076dc",
  "cyclic",             "Cyclical pseudotime",   "Cyclic",    "#2ec5c5",
  "end_state_prob",     "End state probability", "Prob",      "#edd100",
  "cluster_assignment", "Cluster assignment",    "Cluster",   "#ffa500",
  "orth_proj",          "Orthogonal projection", "Proj",      "#2ecc40",
  "cell_graph",         "Cell graph",            "Cell",      "#a000dc"
)

usethis::use_data(wrapper_types)
