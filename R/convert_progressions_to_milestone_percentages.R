#' Convert progressions to milestone percentages
#' @param cell_ids Vector of all cell ids
#' @param milestone_ids Vector of milestone ids
#' @param milestone_network Milestone network
#' @param progressions Progressions dataframe
#' @export
convert_progressions_to_milestone_percentages <- function(cell_ids, milestone_ids, milestone_network, progressions) {
  check_froms <- progressions %>% group_by(cell_id) %>% summarise(n = length(unique(from)))
  if (any(check_froms$n > 1)) {
    stop("In ", sQuote("progressions"), ", cells should only have 1 unique from milestone.")
  }

  froms <- progressions %>%
    group_by(cell_id) %>%
    summarise(
      milestone_id = from[[1]],
      percentage = 1 - sum(percentage)
    )

  tos <- progressions %>%
    select(cell_id, milestone_id = to, percentage)

  bind_rows(froms, tos)
}
