#' @rdname add_pseudotime
#' @export
calculate_pseudotime <- function(trajectory) {
  if(!"root_milestone_id" %in% trajectory) {
    trajectory <- add_root(trajectory)
  }

  root_cell_id <- trajectory$milestone_percentages %>%
    filter(milestone_id == trajectory$root_milestone_id) %>%
    arrange(desc(percentage)) %>%
    pull(cell_id) %>%
    first()

  if(is.na(root_cell_id)) {stop("Could not find rooting cell for pseudotime calculation")}

  pseudotime <- compute_tented_geodesic_distances(trajectory, root_cell_id)[1, ]

  pseudotime
}


#' Add or calculate pseudotime as distance from the root
#'
#' @param trajectory The trajectory
#' @param pseudotime Named vector containing the pseudotime for every cell
#' @export
add_pseudotime <- function(trajectory, pseudotime = NULL) {
  if (is.null(pseudotime)) {
    pseudotime <- calculate_pseudotime(trajectory)
  }

  testthat::expect_true(length(names(pseudotime) )== length(trajectory$cell_ids))
  testthat::expect_setequal(names(pseudotime), trajectory$cell_ids)

  trajectory$pseudotime <- pseudotime[trajectory$cell_ids]
  trajectory
}


# Process pseudotime from file ---------------------------
process_pseudotime <- function(model, dir_output) {
  pseudotime <- read_pseudotime(dir_output)
  model %>% add_pseudotime(pseudotime)
}


output_processors <- output_processors %>% add_row(
  id="pseudotime",
  processor=list(process_pseudotime),
  required_files=list(c("pseudotime.csv")),
  optional_files=list(c()),
  required_output=list(c()),
  description="Add pseudotime, a single value for every cell which denotes its progression from start to finish",
  creates_trajectory=FALSE
)

read_pseudotime <- function(dir_output) {
  read_assignment(
    file.path(dir_output, "pseudotime.csv"),
    readr::cols(cell_id=readr::col_character(), pseudotime=readr::col_double())
  )
}

#' @rdname add_pseudotime
#' @param dir_output The output directory
#' @export
write_pseudotime <- function(pseudotime, dir_output) {
  tibble(cell_id = names(pseudotime), pseudotime = pseudotime) %>%
    write_csv(file.path(dir_output, "pseudotime.csv"))
}
