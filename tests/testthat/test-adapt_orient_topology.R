context("Testing orientation of trajectories")

test_that("flip_edges works correctly", {
  cell_ids <- c("a", "b", "c", "d", "e")

  milestone_network <- tibble::tribble(
    ~from, ~to, ~length, ~directed,
    "B", "A", 1, TRUE,
    "B", "C", 1, TRUE
  )
  progressions <- tibble::tribble(
    ~cell_id, ~from, ~to, ~percentage,
    "a", "B", "A", 1,
    "b", "B", "A", 0.5,
    "c", "B", "A", 0,
    "d", "B", "C", 0.5,
    "e", "B", "C", 1
  )

  trajectory <- wrap_data(
    cell_ids = cell_ids
  ) %>%
    add_trajectory(milestone_network = milestone_network, progressions = progressions)

  trajectory$dimred_segment_progressions <- tribble(
    ~from, ~to, ~percentage,
    "B", "A", 0,
    "B", "C", 1
  )

  trajectory_flipped <- flip_edges(trajectory, milestone_network %>% filter(from == "B", to == "A"))

  expect_true(all(
    c("A->B", "B->C") %in%
      paste0(trajectory_flipped$milestone_network$from, "->", trajectory_flipped$milestone_network$to))
  )
  expect_false(all(
    c("B->A", "C->B") %in%
      paste0(trajectory_flipped$milestone_network$from, "->", trajectory_flipped$milestone_network$to))
  )

  expect_true(
    all(trajectory_flipped$dimred_segment_progressions$percentage == 1)
  )
})




test_that("orient_topology_to_velocity orients a linear trajectory correctly", {
  # we'll use a simple linear trajectory
  cell_ids <- c("a", "b", "c", "d", "e")
  pseudotime <- setNames(seq_along(cell_ids), cell_ids)
  expression <- as.matrix(data.frame(
    a = pseudotime,
    b = pseudotime ** 2,
    c = log(pseudotime)
  ))
  expression_future <- as.matrix(data.frame(
    a = (pseudotime + 1),
    b = (pseudotime + 1) ** 2,
    c = log(pseudotime + 1)
  ))

  # the milestone network is "wrong" in the sense that B and A are wrongly oriented
  milestone_network <- tibble::tribble(
    ~from, ~to, ~length, ~directed,
    "B", "A", 1, TRUE,
    "B", "C", 1, TRUE
  )
  progressions <- tibble::tribble(
    ~cell_id, ~from, ~to, ~percentage,
    "a", "B", "A", 1,
    "b", "B", "A", 0.5,
    "c", "B", "A", 0,
    "d", "B", "C", 0.5,
    "e", "B", "C", 1
  )

  trajectory <- wrap_expression(
    counts = expression,
    expression = expression,
    expression_future = expression_future
  ) %>%
    add_trajectory(milestone_network = milestone_network, progressions = progressions)

  # TODO: move to scvelo package or re-enable this part of the test?
  # # orient the trajectory
  # trajectory_oriented <- dynwrap::orient_topology_to_velocity(trajectory)
  #
  # # make sure the first edge is correctly oriented
  # expect_true("A->B" %in% paste0(trajectory_oriented$milestone_network$from, "->", trajectory_oriented$milestone_network$to))
  # expect_false("B->A" %in% paste0(trajectory_oriented$milestone_network$from, "->", trajectory_oriented$milestone_network$to))
})
