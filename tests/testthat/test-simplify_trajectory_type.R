context("Testing simplify_trajectory_type")

from <- c(
  "undirected_cycle",
  "directed_cycle",
  "directed_cycle",
  "bifurcation",
  "unknown",
  "unrooted_tree",
  "cycle",
  "simple_fork",
  "acyclic_graph"
)
to <- c(
  "cycle",
  "cycle",
  "cycle",
  "bifurcation",
  "unknown",
  "tree",
  "cycle",
  "bifurcation",
  "acyclic_graph"
)

test_that("Test simplify trajectory type", {
  expect_equal(simplify_trajectory_type(from), to)
  expect_equal(do.call(simplify_trajectory_type, as.list(from)), to)
  expect_true(is.na(simplify_trajectory_type("not a trajectory type")))
})
