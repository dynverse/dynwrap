context("Testing get_cell_grouping")


test_that("Testing get_cell_grouping", {
  milestone_percentages <- data_frame(
    cell_id = rownames(x),
    z = seq(-1, 1, length.out = length(cell_id)),
    M1 = ifelse(z <= 0, -z, 0),
    M2 = 1 - abs(z),
    M3 = ifelse(z >= 0, z, 0)
  ) %>%
    select(-z) %>%
    gather(milestone_id, percentage, -cell_id)

  cell_grouping <- get_cell_grouping(milestone_percentages) %>%
    full_join(milestone_percentages %>% spread(milestone_id, percentage), by = "cell_id") %>%
    mutate(control = case_when(.$M1 >= .5 ~ "M1", .$M3 >= .5 ~ "M3", TRUE ~ "M2"))

  expect_true(cell_grouping %>% {.$group_id == .$control} %>% all)
})
