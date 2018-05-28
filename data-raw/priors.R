library(tibble)
priors <- tribble(
  ~prior_id, ~prior_name, ~prior_description,
  "start_id", "Start cell", "Vector containing ids of the start cells",
  "end_id", "End cell(s)", "Vector containing ids of the end cells",
  "end_n", "# end states", "The number of end states",
  "start_n", "# start states", "The number of start states",
  "states_id", "Cell clustering", "Named character vector linking the cell ids to different states or clustering",
  "states_n", "# states", "Vector containing the number of end states (including start, end and intermediary states)",
  "states_network", "State network", "Dataframe containing the known network between states. Contains a from and to column",
  "time_id", "Time course", "Named numeric vector linking the cell ids to time points",
  "genes_id", "Marker genes", "Character vector with genes known to be important in the dynamic process"
)

prior_usages <- tribble(
  ~prior_usage, ~color,
  "optional", "#0074D9",
  "no", "#EEEEEE",
  "required", "#FF4136"
)

devtools::use_data(priors, prior_usages, overwrite = TRUE)