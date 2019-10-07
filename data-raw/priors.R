library(tibble)

priors <- tribble(
  ~prior_id, ~name, ~description, ~type,
  "start_id", "Start cell(s)", "One or more start cell identifiers", "soft",

  "end_id", "End cell(s)", "One or more end cell identifiers", "soft",

  "end_n", "# end states", "The number of end states", "soft",

  "start_n", "# start states", "The number of start states", "soft",

  "leaves_n", "# leaves", "The number of leaves", "soft",

  "groups_id", "Cell clustering", "Named character vector linking the cell identifiers to different states/branches", "hard",

  "groups_n", "# states", "Number of states/branches, including start, end and intermediary states", "soft",

  "groups_network", "State network", "Dataframe containing the known network between states/branches. Contains a from and to column", "hard",

  "timecourse_continuous", "Time course (continuous)", "Named numeric vector linking the cell ids to time points", "hard",

  "timecourse_discrete", "Time course (discrete)", "Named numeric vector linking the cell ids to time course points", "hard",

  "features_id", "Marker genes", "Genes/features known to be important in the dynamic process", "soft",

  "dataset", "The full dataset", "The full dataset, including (if available) the gold standard", "hard"
)

prior_usages <- tribble(
  ~prior_usage, ~color,
  "optional", "#0074D9",
  "no", "#EEEEEE",
  "required", "#FF4136"
)

usethis::use_data(priors, prior_usages, overwrite = TRUE)
