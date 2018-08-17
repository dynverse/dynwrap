#' Wrap the output of a TI method
#'
#' Will extract outputs from a given folder given an output format:
#'  * rds: One `output.rds` file containing all the output.\
#'    - Output can be present as `.$pseudotime` or as `.$linear_trajectory$pseudotime`.
#'    - Parameters specific for an output should be given in `.$linear_trajectory$params`
#'  * text: Csv and/or json files, including subdirectories.
#'    - Output can be present as `./pseudotime.csv` or as `./linear_trajectory/pseudotime.csv`.
#'    - Parameters specific for an output should be given in `./linear_trajectory/params.json`.
#'  * feather: Feather files, including subdirectories.
#'    - Output can be present as `./pseudotime.feather` or as `./linear_trajectory/pseudotime.feather`.
#'    - Parameters specific for an output should be given in `./linear_trajectory/params.json`.
#'  * dynwrap: Directly a dynwrap wrapper as an rds file in `output.rds`
#'
#' @param output_ids The outputs ids expected by the method.
#' @param dir_output The directory containing the output files.
#' @param output_format The output format. Must be one of `"text"`, `"rds"`, `"feather"` or `"dynwrap"`.
#'
#' @export
wrap_output <- function(output_ids, dir_output, output_format = c("text", "rds", "feather", "dynwrap")) {
  output_format <- match.arg(output_format)
  if(output_format == "rds") {
    wrap_rds(output_ids, dir_output)
  } else if (output_format == "text") {
    wrap_text(output_ids, dir_output)
  } else if (output_format == "feather") {
    wrap_feather(output_ids, dir_output)
  } else if (output_format == "dynwrap") {
    read_rds(file.path(dir_output, "output.rds"))
  }
}
