context("Simplify network")

test_that("Testing simplify_igraph_network", {
  net <- tibble::tribble(
    ~from, ~to,
    1, 2,
    2, 3,
    3, 4,
    3, 5,
    4, 5
  )
  gr <- igraph::graph_from_data_frame(net)

  newgr <- simplify_igraph_network(gr)
  newnet <- igraph::as_data_frame(newgr)
  expect_equal(nrow(newnet), 3)
  expect_true( all(c("from", "to", "weight") %in% colnames(newnet)) )
  expect_true( newnet %>% filter(from == "1", to == "3") %>% nrow == 1 )
  expect_true( newnet %>% filter(from == "3", to == "5") %>% nrow == 2 )
})
