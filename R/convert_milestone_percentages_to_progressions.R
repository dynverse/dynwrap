#' Conversion between milestone percentages and progressions
#'
#' @inheritParams wrap_data
#' @inheritParams add_trajectory
#'
#' @return A data frame with columns `cell_id`, `from`, `to`, `percentage`.
#'
#' @seealso [add_trajectory()], [convert_progressions_to_milestone_percentages]
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
#' @export
convert_milestone_percentages_to_progressions <- function(
  cell_ids,
  milestone_ids,
  milestone_network,
  milestone_percentages
) {
  # for cells that have 2 or more milestones
  progr_part1 <-
    milestone_network %>%
    inner_join(milestone_percentages, by = c("to" = "milestone_id")) %>%
    inner_join(milestone_percentages %>% select(cell_id, milestone_id), by = c("from" = "milestone_id", "cell_id")) %>%
    select(cell_id, from, to, percentage)

  # for cells that have just 1 milestone
  milnetdf <- bind_rows(
    milestone_network %>% transmute(milestone_id = to, from, to, percentage = 1),
    milestone_network %>% transmute(milestone_id = from, from, to, percentage = 0)
  )
  milpct_just1 <-
    milestone_percentages %>%
    group_by(cell_id) %>%
    filter(n() == 1)

  progr_part2 <-
    if (nrow(milpct_just1) > 0) {
        milpct_just1 %>%
        select(-percentage) %>%
        left_join(milnetdf, by = "milestone_id") %>%
        filter(percentage == max(percentage)) %>% # prefer rows where percentage == 1
        sample_n(1) %>%
        ungroup() %>%
        select(cell_id, from, to, percentage)
    } else {
      NULL
    }

  progr <-
    bind_rows(progr_part1, progr_part2) %>%
    arrange(match(cell_id, cell_ids))

  assert_that(
    unique(milestone_percentages$cell_id) %all_in% progr$cell_id,
    msg = "Some cells are on edges which are not contained in the milestone network"
  )

  progr
}
