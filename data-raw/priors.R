library(tibble)

priors <- tribble(
  ~prior_id, ~name, ~description, ~prior_id2,
  "start_id", "Start cell(s)", "One or more start cell identifiers", "start_id",

  "end_id", "End cell(s)", "One or more end cell identifiers", "end_id",

  "end_n", "# end states", "The number of end states", "end_n",

  "start_n", "# start states", "The number of start states", "start_n",

  "states_id", "Cell clustering", "Named character vector linking the cell identifiers to different states/branches", "grouping_assignment",

  "states_n", "# states", "Number of states/branches, including start, end and intermediary states", "n_branches",

  "states_network", "State network", "Dataframe containing the known network between states/branches. Contains a from and to column", "grouping_network",

  "time_id", "Time course", "Named numeric vector linking the cell ids to time points", "time",

  "genes_id", "Marker genes", "Genes/features known to be important in the dynamic process", "marker_feature_ids",

  "task", "The full dataset", "The full dataset, including gold standard", "task"
)

prior_usages <- tribble(
  ~prior_usage, ~color,
  "optional", "#0074D9",
  "no", "#EEEEEE",
  "required", "#FF4136"
)

devtools::use_data(priors, prior_usages, overwrite = TRUE)
