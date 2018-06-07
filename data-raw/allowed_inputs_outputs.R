library(tidyverse)
devtools::load_all()

# allowed outputs
requireNamespace("dynwrap")
add_ids <- as.character(lsf.str(asNamespace("dynwrap"))) %>%
  stringr::str_subset("add_.*")

requireNamespace("Rd2roxygen")
allowed_outputs <- map_df(add_ids, function(add_id) {
  file <- paste0("man/", add_id, ".Rd")
  rd <- Rd2roxygen::parse_file(file)

  creates_trajectory <- str_detect(rd$value, ".*The trajectory model.*") && !is.null(rd$value)

  output_id <- gsub("add_(.*)", "\\1", add_id)

  processor <- get_output_processor(output_id)

  tibble(
    output_id = output_id,
    description = rd$title,
    creates_trajectory = creates_trajectory,
    required_params = list(processor$required_params),
    optional_params = list(processor$optional_params)
  )
})

# allowed inputs
data(priors, package = "dynwrap", envir = environment())

allowed_inputs <- tribble(
  ~input_id, ~description,
  "expression", "Expression matrix",
  "counts", "Raw counts matrix"
) %>% bind_rows(
  priors %>% select(input_id = prior_id2, description = description)
)

usethis::use_data(allowed_outputs, allowed_inputs, overwrite = TRUE)
