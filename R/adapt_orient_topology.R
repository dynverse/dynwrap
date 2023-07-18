#' Flip a set of edges of the milestone network
#'
#' @description
#' Note that this will remove associated roots, reroot the trajectory using [add_root()]
#'
#' @inheritParams common_param
#' @param milestone_network_toflip A dataframe with a from and to column, containing the subset of the milestone network #'
#' @keywords adapt_trajectory
#'
#' @return A trajectory object
#'
#' @importFrom methods is
#'
#' @export
flip_edges <- function(
  trajectory,
  milestone_network_toflip
) {
  assert_that(
    is(milestone_network_toflip, "data.frame"),
    c("from", "to") %all_in% colnames(milestone_network_toflip)
  )
  assert_that(all(
    paste0(milestone_network_toflip$from, milestone_network_toflip$to) %in%
      paste0(trajectory$milestone_network$from, trajectory$milestone_network$to)
  ), msg = "All edges in the milestone_network_toflip should also be present in the trajectory milestone network")

  milestone_network_toflip <- milestone_network_toflip %>%
    select(from, to)

  # flip edge if from is later than to
  trajectory$milestone_network <- trajectory$milestone_network %>%
    left_join(milestone_network_toflip %>% mutate(flip = TRUE), c("from", "to")) %>%
    mutate(flip = ifelse(is.na(flip), FALSE, flip))

  # flip milestone network & progressions
  trajectory$progressions <- trajectory$progressions %>%
    left_join(trajectory$milestone_network %>% select(from, to, flip), c("from", "to")) %>%
    mutate(
      from2 = from,
      from = ifelse(flip, to, from),
      to = ifelse(flip, from2, to),
      percentage = ifelse(flip, 1-percentage, percentage)
    ) %>%
    select(-flip, -from2)

  if (!is.null(trajectory$dimred_segment_progressions)) {
    trajectory$dimred_segment_progressions <-
      trajectory$dimred_segment_progressions %>%
      left_join(trajectory$milestone_network %>% select(from, to, flip), c("from", "to")) %>%
      mutate(
        from2 = from,
        from = ifelse(flip, to, from),
        to = ifelse(flip, from2, to),
        percentage = ifelse(flip, 1-percentage, percentage)
      ) %>%
      select(-flip, -from2)
  }

  trajectory$milestone_network <- trajectory$milestone_network %>%
    mutate(
      from2 = from,
      from = ifelse(flip, to, from),
      to = ifelse(flip, from2, to),
      directed = TRUE
    ) %>%
    select(-flip, -from2)

  trajectory
}
