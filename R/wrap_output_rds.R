#' @rdname wrap_output
#'
#' @importFrom readr read_rds
wrap_rds <- function(output_ids, dir_output) {
  output <- readr::read_rds(file.path(dir_output, "output.rds"))

  testthat::expect_true("cell_ids" %in% names(output))
  cell_ids <- output$cell_ids
  output$cell_ids <- NULL
  model <- wrap_data(cell_ids = cell_ids)

  # iterate over all promised output ids and
  # append the values to the model
  for (output_id in output_ids) {
    processor <- get_output_processor(output_id)

    # define output from which to extract variables
    output_list <- output

    # also include output[[output_id]] if it is a list and not a dataframe
    if (is.list(output[[output_id]]) && !is.data.frame(output[[output_id]])) {
      output_list <- c(
        output_list,
        output[[output_id]]
      )
    }

    output_list <- output_list[intersect(processor$args, names(output_list))]

    model <- invoke(processor$processor, c(list(model), output_list))
  }

  model
}
