context("Testing simplify_igraph_network")

tests <- list(
  list(
    name = "directed_linear",
    directed = TRUE,
    net = tribble(
      ~from, ~to,
      1, 2,
      2, 3
    ),
    expected_net = tribble(
      ~from, ~to, ~weight, ~directed,
      1, 3, 2, TRUE
    )
  ),
  list(
    name = "directed_diverging",
    directed = TRUE,
    net = tribble(
      ~from, ~to,
      1, 2,
      1, 3
    ),
    expected_net = tribble(
      ~from, ~to, ~weight, ~directed,
      1, 2, 1, TRUE,
      1, 3, 1, TRUE
    )
  ),
  list(
    name = "undirected_linear",
    directed = FALSE,
    net = tribble(
      ~from, ~to,
      1, 2,
      1, 3
    ),
    expected_net = tribble(
      ~from, ~to, ~weight, ~directed,
      2, 3, 2, FALSE
    )
  ),
  list(
    name = "simple_branching",
    directed = TRUE,
    net = tribble(
      ~from, ~to,
      1, 2,
      2, 3,
      2, 4,
      3, 5,
      3, 6
    ),
    expected_net = tribble(
      ~from, ~to, ~weight, ~directed,
      1, 2, 1, TRUE,
      2, 3, 1, TRUE,
      2, 4, 1, TRUE,
      3, 5, 1, TRUE,
      3, 6, 1, TRUE
    )
  ),
  list(
    name = "simple_undirected_branching",
    directed = FALSE,
    net = tribble(
      ~from, ~to,
      1, 2,
      2, 3,
      2, 4,
      3, 5,
      3, 6
    ),
    expected_net = tribble(
      ~from, ~to, ~weight, ~directed,
      1, 2, 1, FALSE,
      2, 3, 1, FALSE,
      2, 4, 1, FALSE,
      3, 5, 1, FALSE,
      3, 6, 1, FALSE
    )
  ),
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
      ~from, ~to, ~weight, ~directed,
      1, 3, 3, TRUE,
      3, 4, 3, TRUE,
      3, 6, 9, TRUE
    )
  ),
  list(
    name = "branching_converging",
    directed = TRUE,
    net = tribble(
      ~from, ~to, ~weight,
      1, 2, 1,
      2, 3, 2,
      3, 4, 3,
      4, 5, 4,
      3, 5, 5
    ),
    expected_net = tribble(
      ~from, ~to, ~weight, ~directed,
      1, 3, 3, TRUE,
      3, 5, 7, TRUE,
      3, 5, 5, TRUE
    )
  ),
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
      ~from, ~to, ~weight, ~directed,
      1, 1, 6, TRUE
    )
  ),
  list(
    name = "undirected_branching_converging",
    directed = FALSE,
    net = tribble(
      ~from, ~to, ~weight,
      1, 2, 1,
      3, 2, 2,
      3, 4, 3,
      5, 4, 4,
      3, 5, 5
    ),
    expected_net = tribble(
      ~from, ~to, ~weight, ~directed,
      1, 3, 3, FALSE,
      3, 3, 12, FALSE
    )
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
      full_join(newnet %>% mutate(right = TRUE), by = c("from", "to", "weight", "directed"))

    pass_check <- all(!is.na(control$left) & !is.na(control$right))

    # cat(ifelse(pass_check, "SUCCEEDED!", "FAILED!"), " ", test$name, " ", ifelse(test$directed, "(directed)", "(undirected)"), "\n", sep = "")
    # cat("Original:\n")
    # print(test$net %>% as.data.frame)
    # cat("Expected:\n")
    # print(test$expected_net %>% as.data.frame)
    # cat("Got:\n")
    # print(newnet)
    # cat("============================\n")
    expect_true( pass_check )
    expect_true( all(c("from", "to", "weight", "directed") %in% colnames(newnet)) )
  })
}

