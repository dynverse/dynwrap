#' @rdname wrap_output
#'
#' @importFrom stringr str_subset
wrap_feather <- function(output_ids, dir_output) {
  # install feather if not available
  dynutils::install_packages("feather", "dynwrap", prompt = TRUE)
  requireNamespace("feather")

  files <- list.files(dir_output, full.names = TRUE)

  # initial model with cell ids
  cell_ids_file <- files %>% str_subset("cell_ids")
  if (length(cell_ids_file) > 1) cell_ids_file <- cell_ids_file %>% first()
  testthat::expect_length(cell_ids_file, 1)
  cell_ids <- feather::read_feather(cell_ids_file) %>% pull(cell_ids)
  model <- wrap_data(cell_ids = cell_ids)

  # iterate over all promised output ids and
  # append the values to the model
  for (output_id in output_ids) {
    output_list <- list()

    processor <- get_output_processor(output_id)
    inner_files <- list.files(file.path(dir_output, output_id), all.files = TRUE)

    for(arg in processor$args) {
      matching <- stringr::str_subset(c(files, inner_files), glue::glue(".*\\/{arg}.feather"))
      if(length(matching) > 0) {
        output_list[[arg]] <- feather::read_feather(first(matching))
      }
    }

    # also add extra params, both from the output_id folder as well as from the main folder
    if(file.exists(file.path(dir_output, output_id, "params.json"))) {
      output_list <- c(
        output_list,
        jsonlite::read_json(file.path(dir_output, output_id, "params.json"))
      )
    }

    # feather can only save tibbles, if something else needs to be saved it will be saved as a one-column tibble with colname equal to output_id
    # here we simplify this again to a vector
    # alternatively: if we need a named vector, the tibble will contain a column with "name"
    output_list <- map2(output_list, names(output_list), function(x, name) {
      if(is.data.frame(x) && ncol(x) == 1 && colnames(x) == name) {
        x[[name]]
      } else if (is.data.frame(x) && ncol(x) == 2 && c("name", name) %in% colnames(x)) {
        set_names(x[[name]], x[["name"]])
      } else {
        x
      }
    })

    # use processor to extend model with the provided data
    model <- invoke(processor$processor, c(list(model), output_list))
  }

  model
}
