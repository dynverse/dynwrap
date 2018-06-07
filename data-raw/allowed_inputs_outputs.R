library(tidyverse)
devtools::load_all()

# allowed outputs ---------------------------------------

# all functions which start with an "add", but filtered by:
# if they contain "The trajectory model." as return value in documentation
# or if they are one of:
output_ids_builds_upon_trajectory <- c("root", "pseudotime", "waypoints", "dimred")

requireNamespace("dynwrap")
add_ids <- as.character(lsf.str(asNamespace("dynwrap"))) %>%
  stringr::str_subset("add_.*")

requireNamespace("Rd2roxygen")
allowed_outputs <- map_df(add_ids, function(add_id) {
  output_id <- gsub("add_(.*)", "\\1", add_id)

  file <- paste0("man/", add_id, ".Rd")
  rd <- Rd2roxygen::parse_file(file)

  creates_trajectory <- str_detect(rd$value, ".*The trajectory model.*") && !is.null(rd$value)
  builds_upon_trajectory <- output_id %in% output_ids_builds_upon_trajectory

  processor <- get_output_processor(output_id)

  if (creates_trajectory || builds_upon_trajectory) {
    tibble(
      output_id = output_id,
      description = rd$title,
      creates_trajectory = creates_trajectory,
      required_args = list(processor$required_args),
      optional_args = list(processor$optional_args)
    )
  } else {
    tibble()
  }
})

# allowed inputs --------------------------------
# will use expression/counts and all priors EXCEPT task
data(priors, package = "dynwrap", envir = environment())

allowed_inputs <- tribble(
  ~input_id, ~description,
  "expression", "Expression matrix",
  "counts", "Raw counts matrix"
) %>% bind_rows(
  priors %>% select(input_id = prior_id2, description = description)
) %>%
  filter(
    !input_id %in% c("task")
  )

usethis::use_data(allowed_outputs, allowed_inputs, overwrite = TRUE)
