.container_save_inputs <- function(
  envir,
  dir_input,
  input_format = c("hdf5", "text", "rds"),
  input_ids = NULL,
  param_ids = NULL
) {
  input_format <- match.arg(input_format)

  params <- map(param_ids, get, envir) %>% set_names(param_ids)
  inputs <- map(input_ids, get, envir) %>% set_names(input_ids) %>% discard(is.null)

  # save data depending on the input_format
  if (input_format == "text") {
    for (input_id in names(inputs)) {
      input <- inputs[[input_id]]
      write_text_infer(input, glue::glue("{dir_input}/{input_id}"))
    }
  } else if (input_format == "rds") {
    write_rds(inputs, file.path(dir_input, "data.rds"))
  } else if (input_format == "hdf5") {
    # install hdf5r if not available
    dynutils::install_packages("hdf5r", "dynwrap", prompt = TRUE)
    requireNamespace("hdf5r")

    file <- hdf5r::H5File$new(file.path(dir_input, "data.h5"), "w")
    purrr::walk2(inputs, names(inputs), function(x, name) {
      file$create_dataset(name, x)

      if(is.matrix(x)) {
        file$create_dataset(paste0(name, "_rows"), rownames(x))
        file$create_dataset(paste0(name, "_cols"), colnames(x))
      }
    })
    file$close_all() # important to do a close_all here, otherwise some parts of the data can still be open, resulting in invalid h5 files
  }

  # save params as json
  jsonlite::write_json(params, file.path(dir_input, "params.json"), auto_unbox = TRUE)
}

#' @importFrom utils write.csv
write_text_infer <- function(x, path) {
  if(is.matrix(x)) {
    utils::write.csv(x, paste0(path, ".csv"))
  } else if (is.data.frame(x)) {
    readr::write_csv(x, paste0(path, ".csv"))
  } else {
    jsonlite::write_json(x, paste0(path, ".json"))
  }
}
