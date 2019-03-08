#' Transform a list of data objects to a dynwrap trajectory
#'
#' @param output_list A list containing dynwrap data
#' @param output_ids The names of the promised dynwrap objects
#'
#' @export
wrap_output_list <- function(output, output_ids) {
  testthat::expect_true("cell_ids" %in% names(output))
  cell_ids <- output$cell_ids

  model <- wrap_data(
    id = output$id,
    cell_ids = cell_ids,
    cell_info = output$cell_info
  )

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
