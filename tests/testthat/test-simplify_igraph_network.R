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
      3, 3, 12, FALSE,
      3, 1, 3, FALSE
    )
  )
)

for (test in tests) {
  test_that(glue::glue("Testing simplify_igraph_network on {test$name}"), {
    gr <- igraph::graph_from_data_frame(test$net, directed = test$directed)

    newgr <- simplify_igraph_network(gr, allow_duplicated_edges = TRUE)
    newnet <- igraph::as_data_frame(newgr)
    expected <- test$expected_net %>%
      mutate(from = as.character(from), to = as.character(to))

    exp2 <- bind_rows(expected, expected %>% rename(from = to, to = from)) %>% mutate(left = TRUE)
    new2 <- bind_rows(newnet, newnet %>% rename(from = to, to = from)) %>% mutate(right = TRUE)
    control <- full_join(exp2, new2, by = c("from", "to", "weight", "directed"))

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


test_that("simplify_igraph_network's allow_duplicated_edges parameter", {
  net <- data.frame(from = c(1,1,2,3), to = c(2,3,4,4), weight = c(1, 2, 4, 8), directed = TRUE)
  gr <- igraph::graph_from_data_frame(net, directed = TRUE, vertices = 1:4)

  gr1 <- simplify_igraph_network(gr, allow_duplicated_edges = FALSE)
  expect_equal(length(igraph::E(gr1)), 3)
  gr1df <- igraph::as_data_frame(gr1)

  gr2 <- simplify_igraph_network(gr, allow_duplicated_edges = TRUE)
  expect_equal(length(igraph::E(gr2)), 2)
  gr2df <- igraph::as_data_frame(gr2)

  # TODO: expand test to make sure the right weights are being calculated
})

test_that("error is produced when needed", {
  expect_error(simplify_igraph_network(NULL, allow_duplicated_edges = TRUE, edge_points = 1), "allow_dup.*TRUE when edge_p.*NULL")
})


test_that("simplifications with a small cycle works as expected", {
  origdf <- data.frame(from = c("A", "B"), to = c("B", "A"), weight = c(1, 2), directed = T, stringsAsFactors = FALSE)
  gr <- igraph::graph_from_data_frame(origdf)
  edge_points <- tibble(id = c("x", "y"), from = c("A", "B"), to = c("B", "A"), percentage = c(.5, .6))
  totlen <- sum(origdf$weight)

  s1 <- simplify_igraph_network(gr, allow_duplicated_edges = FALSE, allow_self_loops = TRUE, edge_points = edge_points)
  df <- igraph::as_data_frame(s1$gr)
  expect_lt(abs(sum(df$weight) - totlen), .001)
  expect_equal(df, data.frame(from = "A", to = "A", weight = totlen, directed = TRUE, stringsAsFactors = FALSE))
  expect_equal(s1$edge_points[, -4], data.frame(from = c("A", "A"), to = c("A", "A"), id = c("x", "y"), stringsAsFactors = FALSE))
  expect_lt(sum(abs(s1$edge_points$percentage - c(1 / 6, 1 / 3 + .6 * 2 / 3))), .001)

  s2 <- simplify_igraph_network(gr, allow_duplicated_edges = FALSE, allow_self_loops = TRUE)
  df <- igraph::as_data_frame(s2)
  expect_lt(abs(sum(df$weight) - totlen), .001)
  expect_equal(df, data.frame(from = "A", to = "A", weight = totlen, directed = TRUE, stringsAsFactors = FALSE))

  s3 <- simplify_igraph_network(gr, allow_duplicated_edges = TRUE, allow_self_loops = FALSE)
  df <- igraph::as_data_frame(s3)
  expect_lt(abs(sum(df$weight) - totlen), .001)
  expect_equal(df, origdf)
})



test_that("simplifications with a large cycle works as expected", {
  origdf <- data.frame(from = c("A", "B", "C", "D"), to = c("B", "C", "D", "A"), weight = c(1, 2, 3, 4), directed = F, stringsAsFactors = FALSE)
  gr <- igraph::graph_from_data_frame(origdf, directed = any(origdf$directed))
  edge_points <- tibble(id = c("x", "y", "z"), from = c("A", "B", "D"), to = c("B", "C", "A"), percentage = c(.5, .6, 1))
  totlen <- sum(origdf$weight)

  s1 <- simplify_igraph_network(gr, allow_duplicated_edges = FALSE, allow_self_loops = TRUE, edge_points = edge_points)
  df <- igraph::as_data_frame(s1$gr)
  expect_lt(abs(sum(df$weight) - totlen), .001)
  expect_equal(df, data.frame(from = "A", to = "A", weight = totlen, directed = FALSE, stringsAsFactors = FALSE))
  expect_equal(s1$edge_points[, -4], data.frame(from = c("A", "A", "A"), to = c("A", "A", "A"), id = c("x", "y", "z"), stringsAsFactors = FALSE))
  expect_lt(sum(abs(s1$edge_points$percentage - c(1 * .5 / totlen, (1 + .6 * 2) / totlen, 1))), .001)

  s2 <- simplify_igraph_network(gr, allow_duplicated_edges = FALSE, allow_self_loops = TRUE)
  df <- igraph::as_data_frame(s2)
  expect_lt(abs(sum(df$weight) - totlen), .001)
  expect_equal(df, data.frame(from = "A", to = "A", weight = sum(origdf$weight), directed = FALSE, stringsAsFactors = FALSE))

  s3 <- simplify_igraph_network(gr, allow_duplicated_edges = TRUE, allow_self_loops = FALSE)
  df <- igraph::as_data_frame(s3)
  expect_lt(abs(sum(df$weight) - totlen), .001)
  expect_equal(df, data.frame(from = c("A", "A", "B"), to = c("B", "D", "D"), weight = c(1, 4, 5), directed = FALSE, stringsAsFactors = FALSE))
})


test_that("edge case failure", {
  milnet <- tribble(
    ~from, ~to, ~weight, ~directed,
    "D",   "C", 1,       FALSE,
    "C",   "B", 1,       FALSE,
    "B",   "A", 1,       FALSE,
    "D",   "E", 1,       FALSE
  )
  milids <- c("B", "C", "D", "E", "A")
  edgeps <- tribble(
    ~from, ~to, ~percentage, ~id,
    "C",   "B", 0,           "c"
  )

  gr <- igraph::graph_from_data_frame(
    d = milnet,
    directed = any(milnet$directed),
    vertices = milids
  )

  out <- simplify_igraph_network(
    gr,
    allow_duplicated_edges = FALSE,
    allow_self_loops = FALSE,
    force_keep = c(),
    edge_points = edgeps
  )

  expect_lte(abs(out$edge_points$percentage - .5), .001)
})

# TODO: add more tests that check whether combinations of trajectory types, edge_points, allow_self_loops and allow_duplicated edges work correctly

