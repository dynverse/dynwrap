.method_extract_inputs <- function(
  dataset,
  inputs
) {
  input_ids_dataset <-
    inputs %>%
    filter(required, type == "expression") %>%
    pull(input_id)

  map(input_ids_dataset, get_expression, dataset = dataset) %>%
    set_names(input_ids_dataset)
}

#' @importFrom utils data
.method_extract_priors <- function(
  dataset,
  inputs,
  give_priors = NULL
) {
  # extract prior information
  priors <- dataset$prior_information
  priors$dataset <- dataset

  if (!priors %has_names% give_priors) {
    warning("Unknown priors requested: ", paste(setdiff(give_priors, names(priors)), collapse = ", "))
  }

  # required, check if the prior infirm
  required_prior_ids <-
    inputs %>%
    filter(required, type == "prior_information") %>%
    pull(input_id)

  if (!all(required_prior_ids %in% names(priors))) {
    # construct informative error message for missing priors
    missing_priors <- setdiff(required_prior_ids, names(priors))
    missing_priors_text <- glue::glue_collapse(crayon::bold(missing_priors), sep = ", ", last = " and ")

    add_prior_information_params_text <- glue::glue("{missing_priors} = <prior>") %>% glue::glue_collapse(", ")
    add_prior_information_text <- crayon::italic(glue::glue("add_prior_information(dataset, {add_prior_information_params_text})"))

    stop(
      glue::glue(
        "Prior information {missing_priors_text} is missing from dataset {dataset$id} but is required by the method. \n",
        "   -> If known, you can add this prior information using {add_prior_information_text}. \n",
        "   -> Otherwise, this method cannot be used.",
        .trim = FALSE
      )
    )
  }

  args_required_priors <- priors[required_prior_ids]

  # optional
  optional_prior_ids <-
    inputs %>%
    filter(!required, type == "prior_information", input_id %in% give_priors) %>%
    pull(input_id)

  if (!all(optional_prior_ids %in% names(priors))) {
    warning(
      "Prior information ",
      paste(setdiff(optional_prior_ids, names(priors)), collapse = ";"),
      " is optional, but missing from dataset ",
      dataset$id,
      ". Will not give this prior to method.",
      "\n"
    )
  }

  args_optional_priors <- priors[intersect(optional_prior_ids, names(priors))]

  # output
  c(
    args_required_priors,
    args_optional_priors
  )
}
