#' Helper function for processing divergence regions
#'
#' This function returns the combinations between
#' the start of each divergence region and pairwise combinations
#' of the end milestones.
#'
#' @param divergence_regions A divergence regions data frame as produced by `add_trajectory`.
#'
#' @export
get_divergence_triangles <- function(divergence_regions) {
  map_df(unique(divergence_regions$divergence_id), function(did) {
    rel_did <- divergence_regions %>% filter(divergence_id == did)

    fr <- rel_did %>% filter(is_start) %>% pull(milestone_id)
    tos <- rel_did %>% filter(!is_start) %>% pull(milestone_id)

    crossing(
      node1 = tos,
      node2 = tos
    ) %>%
      filter(node1 > node2) %>%
      mutate(
        divergence_id = did,
        start = fr
      ) %>%
      select(divergence_id, start, node1, node2)
  })
}
