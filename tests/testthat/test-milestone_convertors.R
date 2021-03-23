context("Testing percentage convertors")

# cell 'k' is not part of any progressions or percentages, on purpose.
cell_ids <- letters[1:11]
milestone_ids <- LETTERS[1:4]
milestone_network <- tribble(
  ~from, ~to, ~length, ~directed,
  "A",   "B", 1,       TRUE,
  "B",   "C", 2,       TRUE,
  "B",   "D", 3,       TRUE
)
milestone_percentages <- tribble(
  ~cell_id, ~milestone_id, ~percentage,
  "a",      "A",           1,

  "b",      "A",           .25,
  "b",      "B",           .75,

  "c",      "A",           0,
  "c",      "B",           1,

  "d",      "B",           .3,
  "d",      "C",           .7,

  "e",      "B",           .7,
  "e",      "D",           .3,

  "f",      "B",           1,

  "g",      "B",           .3,
  "g",      "C",           .4,
  "g",      "D",           .3,

  "h",      "C",           1,

  "i",      "B",           1,

  "j",      "B",           0,
  "j",      "C",           .5,
  "j",      "D",           .5
)
progressions <- tribble(
  ~cell_id, ~from, ~to, ~percentage,
  "a",      "A",   "B", 0,
  "b",      "A",   "B", .75,
  "c",      "A",   "B", 1,
  "d",      "B",   "C", .7,
  "e",      "B",   "D", .3,
  "f",      "A",   "B", 1,
  "g",      "B",   "C", .4,
  "g",      "B",   "D", .3,
  "h",      "B",   "C", 1,
  "i",      "A",   "B", 1,
  "j",      "B",   "C", .5,
  "j",      "B",   "D", .5
)

test_that("Testing convert_milestone_percentages_to_progressions", {
  progressions_calc <- convert_milestone_percentages_to_progressions(cell_ids, milestone_ids, milestone_network, milestone_percentages)
  prog_control <- progressions %>% rename(orig = percentage) %>%
    full_join(progressions_calc %>% rename(calc = percentage), by = c("cell_id", "from", "to")) %>%
    mutate(
      diff = abs(orig - calc),
      check = !is.na(orig) & !is.na(calc) & diff < 1e-6
    )
  expect_true(all(prog_control$check))

  # expect error because cells are positioned on edges that are not in the milestone_network
  expect_error(convert_milestone_percentages_to_progressions(cell_ids, milestone_ids, milestone_network %>% slice(-1), milestone_percentages))
})

test_that("Testing convert_progressions_to_milestone_percentages", {
  milestone_percentages_calc <- convert_progressions_to_milestone_percentages(cell_ids, milestone_ids, milestone_network, progressions)
  perc_control <- milestone_percentages %>% rename(orig = percentage) %>%
    full_join(milestone_percentages_calc %>% rename(calc = percentage), by = c("cell_id", "milestone_id")) %>%
    mutate(
      diff = abs(orig - calc),
      orig_check = !is.na(orig) | calc == 0,
      check = !is.na(calc) & orig_check & ((is.na(orig) & calc == 0) | diff < 1e-6)
    )
  expect_true(all(perc_control$check))

  # expect error because cells are position on edges that are not in the milestone_network
  expect_error(convert_progressions_to_milestone_percentages(cell_ids, milestone_ids, milestone_network %>% slice(-1), progressions))

  # expect error because cell k is in two different 'from' progressions.
  expect_error(convert_progressions_to_milestone_percentages(
    cell_ids, milestone_ids, milestone_network,
    progressions %>% add_row(cell_id = c("k", "k"), from = c("A", "B"), to = c("C", "D"), percentage = c(.1, .2))
  ))
})
