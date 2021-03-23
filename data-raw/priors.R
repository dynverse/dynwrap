library(tibble)

priors <- tribble(
  ~prior_id, ~name, ~description, ~type, ~format, ~example,
  "start_id", "Start cell(s)", "One or more start cell identifiers", "soft", "character vector", "C1,C2,C3",

  "end_id", "End cell(s)", "One or more end cell identifiers", "soft", "character vector", "C1,C2,C3",

  "end_n", "# end states", "The number of end states", "soft", "integer", "1",

  "start_n", "# start states", "The number of start states", "soft", "integer", "4",

  "leaves_n", "# leaves", "The number of leaves", "soft", "integer", "5",

  "groups_id", "Cell clustering", "Named character vector linking the cell identifiers to different states/branches", "hard", "named character vector", "C1=A,C2=B,C3=B",

  "groups_n", "# states", "Number of states/branches, including start, end and intermediary states", "soft", "integer", "5",

  "groups_network", "State network", "Dataframe containing the known network between states/branches. Contains a from and to column", "hard", "dataframe(from: character, to: character)", "A,B;B,C;B,D",

  "timecourse_continuous", "Time course (continuous)", "Named numeric vector linking the cell ids to time points", "hard", "named double vector", "C1=0.1,C2=0.4,C3=0.8",

  "timecourse_discrete", "Time course (discrete)", "Named numeric vector linking the cell ids to time course points", "hard", "named integer vector", "C1=1,C2=4,C3=7",

  "features_id", "Marker genes", "Genes/features known to be important in the dynamic process", "soft", "character vector", "G1,G2,G3",

  "dataset", "The full dataset", "The full dataset, including (if available) the gold standard", "hard", "dynwrap::wrap_data(...)", "dataset.rds/loom/h5",

  "dimred", "A dimensionality reduction", "A dimensionality reduction of the cells", "soft", "named matrix", "named matrix[num cells x num dimensions¨]"
)

prior_usages <- tribble(
  ~prior_usage, ~color,
  "optional", "#0074D9",
  "no", "#EEEEEE",
  "required", "#FF4136"
)

usethis::use_data(priors, prior_usages, overwrite = TRUE)
