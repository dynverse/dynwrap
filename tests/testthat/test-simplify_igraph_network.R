context("Testing simplify_igraph_network")

tests <- list(
  list(
    name = "branching",
    directed = TRUE,
    net = tribble(
      ~from, ~to, ~weight,
      1, 2, 1,
      2, 3, 2,
      3, 4, 3,
      3, 5, 4,
      5, 6, 5
    ),
    expected_net = tribble(
      ~from, ~to, ~weight,
      1, 3, 3,
      3, 4, 3,
      3, 6, 9
    )
  ),
  # list(
  #   name = "branching_converging",
  #   directed = TRUE,
  #   net = tribble(
  #     ~from, ~to, ~weight,
  #     1, 2, 1,
  #     2, 3, 2,
  #     3, 4, 3,
  #     4, 5, 4,
  #     3, 5, 5
  #   ),
  #   expected_net = tribble(
  #     ~from, ~to, ~weight,
  #     1, 3, 3,
  #     3, 5, 7,
  #     3, 5, 5
  #   )
  # ),
  list(
    name = "cyclical",
    directed = TRUE,
    net = tribble(
      ~from, ~to, ~weight,
      1, 2, 1,
      2, 3, 2,
      3, 1, 3
    ),
    expected_net = tribble(
      ~from, ~to, ~weight,
      1, 1, 6
    )
  # ),
  # list(
  #   name = "undirected_branching_converging",
  #   directed = FALSE,
  #   net = tribble(
  #     ~from, ~to, ~weight,
  #     1, 2, 1,
  #     3, 2, 2,
  #     3, 4, 3,
  #     5, 4, 4,
  #     3, 5, 5
  #   ),
  #   expected_net = tribble(
  #     ~from, ~to, ~weight,
  #     1, 3, 3,
  #     3, 3, 12
  #   )
  )
)

for (test in tests) {
  test_that(glue::glue("Testing simplify_igraph_network on {test$name}"), {
    gr <- igraph::graph_from_data_frame(test$net, directed = test$directed)

    newgr <- simplify_igraph_network(gr)
    newnet <- igraph::as_data_frame(newgr)

    control <-
      test$expected_net %>%
      mutate(from = as.character(from), to = as.character(to), left = TRUE) %>%
      full_join(newnet %>% mutate(right = TRUE), by = c("from", "to", "weight"))

    pass_check <- all(!is.na(control$left) & !is.na(control$right))

    # if (!pass_check) {
    #   cat("Failed ", test$name, " ", ifelse(test$directed, "(directed)", "(undirected)"), "\n", sep = "")
    #   cat("Original:\n")
    #   print(test$net)
    #   cat("Expected:\n")
    #   print(test$expected_net)
    #   cat("Got:\n")
    #   print(newnet)
    #   cat("============================\n")
    # }
    expect_true( pass_check )
    expect_true( all(c("from", "to", "weight") %in% colnames(newnet)) )
  })
}

