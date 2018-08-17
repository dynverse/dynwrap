#' @rdname wrap_output
#'
#' @importFrom testthat expect_true
#' @importFrom readr read_rds
wrap_dynwrap <- function(output_ids, dir_output) {
  model <- readr::read_rds(file.path(dir_output, "output.rds"))

  testthat::expect_true(is_data_wrapper(model))

  for (output_id in output_ids) {
    processor <- get_output_processor(output_id)

    testthat::expect_true(
      all(processor$required_args %in% names(model)),
      label = paste0("Not all expected args for output ", output_id, " are present")
    )
  }

  model
}
