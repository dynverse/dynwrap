priors <- tribble(
  ~prior_id, ~prior_name, ~prior_description, ~prior_id2,
  "start_id", "Start cell(s)", "One or more start cell identifiers", "start_cells",

  "end_id", "End cell(s)", "One or more end cell identifiers", "end_cells",

  "end_n", "# end states", "The number of end states", "n_end_states",

  "start_n", "# start states", "The number of start states", "n_start_states",

  "states_id", "Cell clustering", "Named character vector linking the cell identifiers to different states/branches", "grouping_assignment",

  "states_n", "# states", "Number of states/branches, including start, end and intermediary states", "n_branches",

  "states_network", "State network", "Dataframe containing the known network between states/branches. Contains a from and to column", "grouping_network",

  "time_id", "Time course", "Named numeric vector linking the cell ids to time points", "time",

  "genes_id", "Marker genes", "Genes/features known to be important in the dynamic process", "marker_features_id",

  "task", "The full dataset", "The full dataset, including gold standard", "task"
)

prior_processor <- function(x, input_id, dir_input) {
  path <- file.path(dir_input, "prior_information.json")
  if(file.exists(path)) {
    priors <- jsonlite::read_json(path)
  } else {
    priors <- list()
  }

  priors[[input_id]] <- x

  jsonlite::write_json(priors, path)
}


input_processors <- tribble(
  ~id, ~processor, ~description, ~file,
  "expression",
  function(x, input_id, dir_input) {write.csv(x, file.path(dir_input, "expression.csv"))},
  "The normalised expression",
  "expression.csv",

  "counts",
  function(x, input_id, dir_input) {write.csv(x, file.path(dir_input, "counts.csv"))},
  "The raw counts",
  "counts.csv"
) %>%
  bind_rows(
    priors %>%
      filter(prior_id != "task") %>%
      select(id = prior_id2, description = prior_description) %>%
      mutate(
        processor = list(prior_processor),
        file = "prior_information.json"
      )
  )

save_input <- function(x, input_id, dir_input) {
  input_processors$processor[[which(input_processors$id == input_id)]](x, input_id, dir_input)
}
