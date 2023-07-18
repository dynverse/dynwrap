#' Inferring and adapting single-cell trajectories
#'
#' ![](logo.png)
#'
#' @import dplyr
#' @import tidyr
#' @import dynutils
#' @import readr
#' @import purrr
#' @import assertthat
#' @import dynparam
#' @importFrom tibble is_tibble as_tibble tibble enframe deframe lst tribble rownames_to_column column_to_rownames
#' @importFrom magrittr %<>% %$% set_rownames set_colnames
#' @importFrom glue glue
#'
#' @docType package
#' @name dynwrap
NULL



#' Common param
#'
#' These parameters are commonly used in dynwrap.
#'
#' @param trajectory The trajectory as created by [infer_trajectory()] or [add_trajectory()]
#' @param dataset A dataset created by [wrap_data()] or [wrap_expression()]
#'
#' @keywords internal
#' @name common_param
#' 
#' @return Internal documentation
NULL


# Define valid global variables
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    ".", "branch_id", "cell_id", "comp_1", "comp_2",
    "correlation", "correlation_mean", "directed", "dist",
    "divergence_id", "edge_id", "feature_id", "flip", "from",
    "from_milestone_id", "from_waypoint", "from2", "group_id",
    "housekeeping", "in_divergence", "index", "input_id", "is_start",
    "ix", "label", "length1", "length2", "milestone_id", "new_milestone_id",
    "new_new_milestone_id", "node", "node1", "node2", "num_cells",
    "one", "percentage", "PREDICT", "prior_id", "required", "rowname",
    "sd", "start", "waypoint_id", "weight", "zero", "time", "to", "to_waypoint",
    "triangle_id", "triangle_part", "type", "comp_1_from", "comp_2_from",
    "comp_1_to", "comp_2_to", "str_subset"))
}
