#' Convert milestone percentages to progressions
#' @param cell_ids Vector of all cell ids
#' @param milestone_ids Vector of milestone ids
#' @param milestone_network Milestone network
#' @param milestone_percentages Milestone percentages
#' @export
convert_milestone_percentages_to_progressions <- function(cell_ids, milestone_ids, milestone_network, milestone_percentages) {
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
        relevant_net <- sample_n(relevant_net, 1)
        pct <- 1
      } else {
        # if no such edge was found, look for all edges where from == cid
        relevant_net <- milestone_network %>% filter(from %in% relevant_pct$milestone_id)
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
