library(tibble)

priors <- tribble(
  ~prior_id, ~name, ~description,
  "start_id", "Start cell(s)", "One or more start cell identifiers",

  "end_id", "End cell(s)", "One or more end cell identifiers",

  "end_n", "# end states", "The number of end states",

  "start_n", "# start states", "The number of start states",

  "groups_id", "Cell clustering", "Named character vector linking the cell identifiers to different states/branches",

  "groups_n", "# states", "Number of states/branches, including start, end and intermediary states",

  "groups_network", "State network", "Dataframe containing the known network between states/branches. Contains a from and to column",

  "time", "Time course", "Named numeric vector linking the cell ids to time points",

  "features_id", "Marker genes", "Genes/features known to be important in the dynamic process",

  "task", "The full dataset", "The full dataset, including gold standard"
)

prior_usages <- tribble(
  ~prior_usage, ~color,
  "optional", "#0074D9",
  "no", "#EEEEEE",
  "required", "#FF4136"
)

devtools::use_data(priors, prior_usages, overwrite = TRUE)
