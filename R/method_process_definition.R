.method_process_definition <- function(definition, return_function = TRUE) {
  # check definition
  testthat::expect_true(is.character(definition$id))
  testthat::expect_true(is.character(definition$name))

  # check params
  # TODO: Expand testing of definition, in case 3rd party containers are naughty

  # create params tibble
  if (!is_tibble(definition$parameters)) {
    definition$parameters <- map2_df(
      names(definition$parameters),
      definition$parameters,
      function(id, values) {
        bind_cols(
          tibble(id),
          list_as_tibble(list(values))
        )
      }
    )
  }

  # create inputs tibble
  definition$inputs <-
    data_frame(
      input_id = c(definition$input$required, definition$input$optional, definition$parameters$id),
      required = input_id %in% definition$input$required,
      type = case_when(
        input_id %in% c("counts", "expression") ~ "expression",
        input_id %in% priors$prior_id ~ "prior_information",
        TRUE ~ "parameter"
      )
    )

  definition <- definition %>%
    add_class("dynwrap::ti_method")

  if (!return_function) {
    # create function with which you can instantiate the method with a set of parameters
    param_overrider_fun <- function(...) {
      # get the parameters from this function
      new_defaults <- as.list(environment())[formalArgs(param_overrider_fun)] %>%
        enframe(name = "id", value = "default")

      # remove previous defaults
      definition$parameters <-
        definition$parameters %>%
        select(-default) %>%
        full_join(new_defaults, by = "id")

      definition
    }

    formals(param_overrider_fun) <- get_default_parameters(definition)

    param_overrider_fun
  } else {
    definition
  }
}



#' Tests whether an object is a TI method description
#'
#' @param object The object to be tested
#'
#' @export
is_ti_method <- function(object) {
  ("dynwrap::ti_method" %in% class(object)) || ("dynmethod::ti_method" %in% class(object))
}

#' Get the default parameters of a method
#'
#' @param method A TI method description
#'
#' @export
#'
#' @importFrom testthat expect_true
get_default_parameters <- function(definition) {
  testthat::expect_true(is_ti_method(definition))

  definition$parameters %>%
    select(id, default) %>%
    deframe() %>%
    as.list()
}
