#' This R package contains the code for a common model of single-cell trajectories.
#'
#' @import dplyr
#' @import tidyr
#' @import methods
#' @import dynutils
#' @import readr
#' @import purrr
#' @import assertthat
#' @import dynparam
#' @importFrom tibble is_tibble as_tibble as_data_frame tibble data_frame enframe deframe lst tribble rownames_to_column column_to_rownames
#' @importFrom magrittr %<>% %$% set_rownames set_colnames
#' @importFrom glue glue
#'
#' @docType package
#' @name dynwrap
NULL



#' Common param
#' @param trajectory The trajectory as created by [infer_trajectory()] or [add_trajectory()]
#' @param dataset A dataset created by [wrap_data()] or [wrap_expression()]
#'
#' @keywords internal
#' @name common_param
NULL
