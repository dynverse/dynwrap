library(tibble)
priors <- tribble(
  ~prior_id, ~prior_name, ~prior_description, ~prior_id2,
  "start_id", "Start cell", "Vector containing ids of the start cells", "start_cells",
  "end_id", "End cell(s)", "Vector containing ids of the end cells", "end_cells",
  "end_n", "# end states", "The number of end states", "n_end_states",
  "start_n", "# start states", "The number of start states", "n_start_states",
  "states_id", "Cell clustering", "Named character vector linking the cell ids to different states or clustering", "grouping_assignment",
  "states_n", "# states", "Vector containing the number of end states (including start, end and intermediary states)", "n_branches",
  "states_network", "State network", "Dataframe containing the known network between states. Contains a from and to column", "grouping_network",
  "time_id", "Time course", "Named numeric vector linking the cell ids to time points", "time",
  "genes_id", "Marker genes", "Character vector with genes known to be important in the dynamic process", "marker_features_id",
  "task", "The full dataset", "The full dataset, including gold standard", "task"
)

prior_usages <- tribble(
  ~prior_usage, ~color,
  "optional", "#0074D9",
  "no", "#EEEEEE",
  "required", "#FF4136"
)

devtools::use_data(priors, prior_usages, overwrite = TRUE)
