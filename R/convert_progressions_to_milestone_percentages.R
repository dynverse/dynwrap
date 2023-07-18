#' Conversion between milestone percentages and progressions
#'
#' @inheritParams wrap_data
#' @inheritParams add_trajectory
#'
#' @return A data frame with columns `cell_id`, `milestone_id`, and `percentage`.
#'
#' @seealso [add_trajectory()], [convert_milestone_percentages_to_progressions()]
#'
#' @examples
#' milestone_percentages <- convert_progressions_to_milestone_percentages(
#'   cell_ids = example_trajectory$cell_ids,
#'   milestone_ids = example_trajectory$milestone_ids,
#'   milestone_network = example_trajectory$milestone_network,
#'   progressions = example_trajectory$progressions
#' )
#' head(milestone_percentages)
#'
#' @export
convert_progressions_to_milestone_percentages <- function(
  cell_ids,
  milestone_ids,
  milestone_network,
  progressions
) {

  check_froms <- tapply(progressions$from, progressions$cell_id, function(x) length(unique(x)) == 1)
  if (any(!check_froms)) {
    stop("In ", sQuote("progressions"), ", cells should only have 1 unique from milestone.")
  }

  check_edges <- progressions %>%
    left_join(milestone_network, by = c("from", "to")) %>%
    left_join(milestone_network %>% select(to = from, from = to, length2 = length), by = c("from", "to"))

  if (any(is.na(check_edges$length) & is.na(check_edges$length2))) {
    stop("All from-to combinations in ", sQuote("progressions"), " should be in ", sQuote("milestone_network"), " as well.")
  }

  # determine milestone percentages for self edges
  selfs <- progressions %>%
    filter(from == to) %>%
    select(cell_id, milestone_id = from) %>%
    mutate(percentage = 1)

  progressions <- progressions %>%
    filter(from != to)

  # determine milestone percentages for 'from' milestones
  from_mls <- tapply(progressions$from, progressions$cell_id, first, default = NA_character_)
  from_pct <- 1 - tapply(progressions$percentage, progressions$cell_id, sum, default = NA_real_)
  froms <- tibble(
    cell_id = names(from_mls) %||% character(),
    milestone_id = from_mls[cell_id] %>% unname() %>% as.character(),
    percentage = from_pct[cell_id] %>% unname() %>% as.numeric()
  )

  # determine milestone percentages for 'to' milestones
  tos <- progressions %>%
    select(cell_id, milestone_id = to, percentage)

  # return all percentages
  bind_rows(selfs, froms, tos)
}
