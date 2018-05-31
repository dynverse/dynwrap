input_processors <- tribble(
  ~id, ~processor, ~description, ~file,
  "expression",
  function(x, path) {write.csv(x, paste0(path, ".csv"))},
  "The normalised expression",
  "expression.csv",

  "counts",
  function(x, path) {write.csv(x, paste0(path, ".csv"))},
  "The raw counts",
  "counts.csv",

  "start_cells",
  function(x, path) {write_json(x, paste0(path, ".json"))},
  "List of start cells",
  "start_cells.json"
)

save_input <- function(x, input_id, path) {
  input_processors$processor[[which(input_processors$id == input_id)]](x, path)
}
