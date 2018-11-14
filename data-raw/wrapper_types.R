library(tidyverse)

wrapper_types <- tribble(
  ~id, ~long_name, ~short_name,
  "direct", "Direct", "Direct",
  "linear", "Linear pseudotime", "Linear",
  "cyclic", "Cyclical pseudotime", "Cyclic",
  "end_state_prob", "End state probability", "Prob",
  "cluster_assignment", "Cluster assignment", "Cluster",
  "orth_proj", "Orthogonal projection", "Proj",
  "cell_graph", "Cell graph", "Cell"
)

usethis::use_data(wrapper_types)
