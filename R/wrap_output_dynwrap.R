#' @rdname wrap_output
#'
#' @importFrom testthat expect_true
#' @importFrom readr read_rds
wrap_dynwrap <- function(output_ids, dir_output) {
  model <- readr::read_rds(file.path(dir_output, "output.rds"))

  testthat::expect_true(is_data_wrapper(model))

  model
}
