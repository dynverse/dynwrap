#' @rdname wrap_output
#'
#' @importFrom testthat expect_true
#' @importFrom stringr str_subset
wrap_text <- function(output_ids, dir_output) {
  files <- list.files(dir_output, full.names = TRUE)

  # initial model with cell ids
  cell_ids_file <- files %>% str_subset("cell_ids\\.[^/\\.]*$")
  if (length(cell_ids_file) > 1) cell_ids_file <- cell_ids_file %>% first()
  testthat::expect_length(cell_ids_file, 1)
  cell_ids <- .wrap_text_reader(cell_ids_file)
  model <- wrap_data(cell_ids = cell_ids)

  # iterate over all promised output ids and
  # append the values to the model
  for (output_id in output_ids) {
    output_list <- list()

    processor <- get_output_processor(output_id)
    inner_files <- list.files(file.path(dir_output, output_id), all.files = TRUE)

    # read argument data
    for (argument_name in processor$args) {
      matching_file <- stringr::str_subset(c(files, inner_files), glue::glue(".*\\/{argument_name}\\.[^/]*$"))
      if (length(matching_file) > 0) {
        output_list[[argument_name]] <- .wrap_text_reader(matching_file)
      }
    }

    # also add extra params for this output
    matching <- stringr::str_subset(files, glue::glue(".*{output_id}[_/]params.json"))
    if (length(matching)) {
      output_list <- c(
        output_list,
        jsonlite::read_json(first(matching))
      )
    }

    # use processor to extend model with the provided data
    model <- invoke(processor$processor, c(list(model), output_list))
  }

  model
}


#' @importFrom stringr str_replace
#' @importFrom jsonlite read_json
#' @importFrom readr read_csv
.wrap_text_reader <- function(path) {
  extension <- str_replace(path, ".*\\.([^\\.]*)$", "\\1")
  output_type <- str_replace(path, ".*/([^/]*)\\.[^\\.]*$", "\\1")

  # read as csv or json depending on the extension
  if (extension == "json") {
    specs <- .wrap_text_reader_json_specification
    read_fun <- function(file, specification) specification(jsonlite::read_json(file, simplifyVector = TRUE))
  } else {
    specs <- .wrap_text_reader_csv_specification
    read_fun <- function(file, specification) readr::read_csv(file, col_types = specification)
  }

  if (!output_type %in% names(specs)) {
    stop("No specification for > ", output_type, " with extension ", extension)
  }

  read_fun(path, specs[[output_type]])
}

#' @importFrom readr cols
.wrap_text_reader_csv_specification <- list(
  cell_ids = cols(cell_ids = "c"),
  pseudotime = cols(cell_id = "c", pseudotime = "d"),
  milestone_network = cols(from = "c", to = "c", length = "d", directed = "l"),
  dimred = cols(cell_id = "c", .default = "d"),
  dimred_milestones = cols(milestone_id = "c", .default = "d"),
  divergence_regions = cols(milestone_id = "c", divergence_id = "c", is_start = "l"),
  milestone_percentages = cols(cell_id = "c", milestone_id = "c", percentage = "d"),
  progressions = cols(cell_id = "c", from = "c", to = "c", percentage = "d"),
  branch_network = cols(from = "c", to = "c"),
  branches = cols(branch_id = "c", length = "d", directed = "l"),
  branch_progressions = cols(cell_id = "c", branch_id = "c", percentage = "d"),
  cell_graph = cols(from = "c", to = "c"),
  to_keep = cols(cell_id = "c", to_keep = "l"),
  group_ids = cols(group_ids = "c"),
  grouping = cols(cell_id = "c", group_id = "c"),
  milestone_ids = cols(milestone_ids = "c"),
  end_state_probabilities = cols(cell_id = "c", .default = "d"),
  timings = cols(name = "c", value = "d")
)

.wrap_text_reader_json_helpers <- list(
  character = function(x) unlist(x) %>% as.character(),
  named_character = function(x) unlist(x) %>% {set_names(as.character(.), names(.))},
  named_numeric = function(x) unlist(x) %>% {set_names(as.numeric(.), names(.))},
  named_logical = function(x) unlist(x) %>% {set_names(as.logical(.), names(.))},
  named_integer = function(x) unlist(x) %>% {set_names(as.integer(.), names(.))}
)

.wrap_text_reader_json_specification <- list(
  cell_ids = .wrap_text_reader_json_helpers$character,
  pseudotime = .wrap_text_reader_json_helpers$named_numeric,
  group_ids = .wrap_text_reader_json_helpers$character,
  grouping = .wrap_text_reader_json_helpers$named_character,
  milestone_ids = .wrap_text_reader_json_helpers$character,
  timings = .wrap_text_reader_json_helpers$named_numeric,
  to_keep = function(x) {
    # `x` can be either a named logical vector or a character vector
    y <- unlist(x)
    if (!is.logical(y)) {
      y <- as.character(y)
    }
    y
  }
)
