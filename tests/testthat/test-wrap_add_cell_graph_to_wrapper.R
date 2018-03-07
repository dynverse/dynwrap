# TODO: update test

# test_that("Testing simplify_sample_graph", {
#   edges <- tibble::tribble(
#     ~from, ~to, ~length, ~directed,
#     "A", "B", .5, F,
#     "B", "C", .6, F,
#     "C", "D", .7, F,
#     "D", "E", .8, F,
#     "D", "F", .9, F,
#     "a", "A", .1, F,
#     "b", "B", .1, F,
#     "bb", "B", .08, F,
#     "c", "C", .05, F,
#     "cc", "c", .1, F,
#     "d", "D", .01, F
#   )
#   is_trajectory <- c(A = T, B = T, C = T, D = T, E = T, "F" = T, a = F, b = F, bb = F, c = F, cc = F, d = F)
#   is_directed <- F
#
#   out <- simplify_sample_graph(edges, is_trajectory, is_directed)
#
#   expect_equal(length(out$milestone_ids), 4)
#   expect_equal(nrow(out$milestone_network), 3)
#   expect_equal(nrow(out$progressions), length(is_trajectory))
#
#   expect_equal(out$milestone_ids, paste0("milestone_", c("A", "D", "E", "F")))
#
#   test_strs <- out$milestone_network %>% {paste(.$from, .$to, .$length, .$directed, sep = "|")} %>% sort
#   expected_strs <- c(
#     "milestone_A|milestone_D|1.8|FALSE",
#     "milestone_D|milestone_E|0.8|FALSE",
#     "milestone_D|milestone_F|0.9|FALSE"
#   ) %>% sort
#   expect_equal(test_strs, expected_strs)
#
#   test_strs <- out$progressions %>% {paste(.$cell_id, .$from, .$to, round(.$percentage, 2), sep = "|")} %>% sort
#   expected_strs <- c(
#     "A|milestone_A|milestone_D|0",
#     "B|milestone_A|milestone_D|0.28",
#     "C|milestone_A|milestone_D|0.61",
#     "D|milestone_D|milestone_E|0",
#     "E|milestone_D|milestone_E|1",
#     "F|milestone_D|milestone_F|1",
#     "a|milestone_A|milestone_D|0",
#     "b|milestone_A|milestone_D|0.28",
#     "bb|milestone_A|milestone_D|0.28",
#     "c|milestone_A|milestone_D|0.61",
#     "cc|milestone_A|milestone_D|0.61",
#     "d|milestone_D|milestone_E|0"
#   ) %>% sort
#   expect_equal(test_strs, expected_strs)
# })
