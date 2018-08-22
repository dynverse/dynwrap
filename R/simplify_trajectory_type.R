#' Convert directed trajectory type to simplified versions
#'
#' @param ... trajectory types to simplify
#'
#' @export
#'
#' @examples
#' simplify_trajectory_type("directed_linear", "directed_graph", "binary_tree")
simplify_trajectory_type <- function(...) {
  vec <- unlist(list(...), recursive = TRUE)

  data("trajectory_types", package = "dynwrap", envir = environment())

  map <- bind_rows(
    trajectory_types,
    trajectory_types %>% mutate(id = simplified)
  ) %>%
    select(id, simplified) %>%
    deframe()

  unname(map[vec])
}
