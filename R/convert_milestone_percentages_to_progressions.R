#' Conversion between milestone percentages and progressions
#'
#' @inheritParams wrap_data
#' @inheritParams add_trajectory
#'
#' @return
#' For `convert_milestone_percentages_to_progressions`: The progressions
#' For `convert_progressions_to_milestone_percentages`: The milestone percentages
#'
#' @seealso [add_trajectory()]
#'
#' @examples
#' progressions <- convert_milestone_percentages_to_progressions(
#'   cell_ids = example_trajectory$cell_ids,
#'   milestone_ids = example_trajectory$milestone_ids,
#'   milestone_network = example_trajectory$milestone_network,
#'   milestone_percentages = example_trajectory$milestone_percentages
#' )
#' head(progressions)
#'
#' milestone_percentages <- convert_progressions_to_milestone_percentages(
#'   cell_ids = example_trajectory$cell_ids,
#'   milestone_ids = example_trajectory$milestone_ids,
#'   milestone_network = example_trajectory$milestone_network,
#'   progressions = example_trajectory$progressions
#' )
#' head(milestone_percentages)
#'
#' @rdname convert_milestone_percentages_to_progressions
#'
#' @export
convert_milestone_percentages_to_progressions <- function(
  cell_ids,
  milestone_ids,
  milestone_network,
  milestone_percentages
) {
  bind_rows(lapply(cell_ids, function(cid) {
    relevant_pct <- milestone_percentages %>% filter(cell_id == cid)

    # if it is known which edge or tent the cell cid is part of
    if (nrow(relevant_pct) >= 2) {
      # simply convert it to progressions
      relevant_progr <-
        milestone_network %>%
        filter(from %in% relevant_pct$milestone_id & to %in% relevant_pct$milestone_id) %>%
        left_join(relevant_pct, by = c("to" = "milestone_id")) %>%
        select(cell_id, from, to, percentage)

      start <- relevant_pct$milestone_id %>% setdiff(relevant_progr$to)

      relevant_progr <- relevant_progr %>% filter(from == start)

      # fail check
      if (nrow(relevant_progr) == 0) {
        stop("According to milestone_percentages, cell ", sQuote(cid), " is between milestones ",
             paste(sQuote(relevant_pct$milestone_id), collapse = " and "), ", but this edge does not exist in milestone_network!")
      }

      # else if cid is in exactly one state
    } else if (nrow(relevant_pct) == 1) {

      # look for edges where to == cid
      relevant_net <- milestone_network %>% filter(to %in% relevant_pct$milestone_id)

      if (nrow(relevant_net) > 0) {
        # if one or more such edges were found, sample one randomly
        relevant_net <- relevant_net %>% sample_n(1)
        pct <- 1
      } else {
        # if no such edge was found, look for all edges where from == cid and sample one randomly
        relevant_net <- milestone_network %>%
          filter(from %in% relevant_pct$milestone_id) %>%
          sample_n(1)
        pct <- 0
      }

      # return generated progressions
      relevant_progr <- relevant_net %>%
        mutate(cell_id = cid, percentage = pct) %>%
        select(cell_id, from, to, percentage)

    } else {
      relevant_progr <- NULL
    }

    relevant_progr
  }))

}
