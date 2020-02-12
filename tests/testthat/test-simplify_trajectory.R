context("Testing simplify_trajectory")

test_that("Simple test", {
  id <- "a"
  cell_ids <- c("truth", "universally", "acknowledged", "that", "a", "single")
  extras <- list("man")

  milestone_ids <- c("must", "be", "in", "want")
  milestone_network <- tibble(
    from = milestone_ids,
    to = milestone_ids[c(2,3,4,1)],
    length = c(2, 1, 3, 2),
    directed = TRUE
  )
  progressions <- tribble(
    ~cell_id,       ~from,    ~to, ~percentage,
    "truth",        "must",  "be",   0.3,
    "universally",  "must",  "be",   1.0,
    "acknowledged", "be",    "in",   0.5,
    "that",         "be",    "in",   0.9,
    "a",            "in",    "want", 0.0,
    "single",       "want",  "must", 0.4
  )
  dimred_segment_progressions <- tribble(
     ~from,    ~to, ~percentage,
     "must",  "be",   0.3,
    "must",  "be",   1.0,
    "be",    "in",   0.5,
    "be",    "in",   0.9,
    "in",    "want", 0.0,
    "want",  "must", 0.4
  )

  trajectory<-
    wrap_data(
      id = id,
      cell_ids = cell_ids
    ) %>%
    add_trajectory(
      milestone_ids = milestone_ids,
      milestone_network = milestone_network,
      progressions = progressions
    )
  trajectory$dimred_segment_progressions <- dimred_segment_progressions
  simp <- simplify_trajectory(trajectory)

  # TODO: Add more tests! for more trajectory types! and more parameters!

  expect_true(nrow(trajectory$dimred_segment_progressions) == nrow(simp$dimred_segment_progressions))
})


test_that("Test whether simplify is able to correctly simplify an undirected", {
  id <- "a"
  cell_ids <- c("truth", "universally", "acknowledged", "that", "a", "single")

  milestone_ids <- c("A", "B", "C")
  milestone_network <- tibble(
    from = c("A", "A"),
    to = c("B", "C"),
    length = c(1, 2),
    directed = FALSE
  )
  progressions <- tribble(
    ~cell_id,       ~from, ~to, ~percentage,
    "truth",        "A",   "B", 0.3,
    "universally",  "A",   "C", 1.0,
    "acknowledged", "A",   "B", 0.5,
    "that",         "A",   "C", 0.9,
    "a",            "A",   "B", 0.0,
    "single",       "A",   "C", 0.4
  )

  trajectory<-
    wrap_data(
      id = id,
      cell_ids = cell_ids
    ) %>%
    add_trajectory(
      milestone_ids = milestone_ids,
      milestone_network = milestone_network,
      progressions = progressions
    )
  simp <- simplify_trajectory(trajectory)

  expect_true(all(cell_ids %in% simp$cell_ids))
  expect_true(all(simp$milestone_network$from == "B"))
  expect_true(all(simp$milestone_network$to == "C"))

})




test_that("Test whether simplify is able to correctly simplify an undirected cycle", {
  id <- "round and round and round we go"
  cell_ids <- c("truth", "universally", "acknowledged", "that", "a", "single")

  milestone_ids <- c("A", "B", "C")
  milestone_network <- tibble(
    from = c("A", "B", "C"),
    to = c("B", "C", "A"),
    length = c(1, 2, 3),
    directed = FALSE
  )
  progressions <- tribble(
    ~cell_id,       ~from, ~to, ~percentage,
    "truth",        "A",   "B", 0.3,
    "universally",  "B",   "C", 1.0,
    "acknowledged", "A",   "B", 0.5,
    "that",         "C",   "A", 0.9,
    "a",            "C",   "A", 0.0,
    "single",       "A",   "B", 0.4
  )

  trajectory<-
    wrap_data(
      id = id,
      cell_ids = cell_ids
    ) %>%
    add_trajectory(
      milestone_ids = milestone_ids,
      milestone_network = milestone_network,
      progressions = progressions
    )
  simp <- simplify_trajectory(trajectory)

  expect_true(all(cell_ids %in% simp$cell_ids))
  expect_true(all(simp$milestone_network$from == c("A", "B", "A")))
  expect_true(all(simp$milestone_network$to == c("B", "C", "C")))
})





test_that("Test whether simplify is able to correctly simplify a graph with a dimred", {
  id <- "I am a graph!"
  cell_ids <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")

  milestone_ids <- c("A", "B", "C", "D", "E", "F")
  milestone_network <- tibble(
    from = c("A", "A", "C", "C", "E", "B"),
    to =   c("B", "D", "B", "D", "A", "F"),
    length = c(1, 2, 3, 4, 5, 6),
    directed = FALSE
  )
  progressions <- tribble(
    ~cell_id, ~from, ~to, ~percentage,
    "1", "A", "B", 0.3,
    "2", "A", "D", 1.0,
    "3", "C", "B", 0.5,
    "4", "C", "D", 0.9,
    "5", "E", "A", 0.0,
    "6", "B", "F", 0.4,
    "7","A", "B", 0.3,
    "8","A", "D", 1.0,
    "9","C", "B", 0.5,
    "10","C", "D", 0.9,
    "11","E", "A", 0.0,
    "12","B", "F", 0.4
  )

  dimred <-
    runif(length(cell_ids) * 3) %>%
    matrix(ncol = 3) %>%
    magrittr::set_colnames(c("comp_1", "comp_2", "comp_3")) %>%
    magrittr::set_rownames(cell_ids)
  dimred_milestones <-
    runif(length(milestone_ids) * 3) %>%
    matrix(ncol = 3) %>%
    magrittr::set_colnames(c("comp_1", "comp_2", "comp_3")) %>%
    magrittr::set_rownames(milestone_ids)
  dimred_segment_progressions <-
    milestone_network %>%
    mutate(percentage = map(seq_len(n()), ~ c(0, .5, 1))) %>%
    unnest(percentage) %>%
    select(from, to, percentage)
  dimred_segment_points <-
    dimred_segment_progressions %>%
    pmap(function(from, to, percentage) {
      if (percentage == 0) {
        dimred_milestones[from, , drop = FALSE]
      } else if (percentage == 1) {
        dimred_milestones[to, , drop = FALSE]
      } else {
        (dimred_milestones[from, , drop = FALSE] + dimred_milestones[to, , drop = FALSE])/2 + runif(3, -.1, .1)
      }
    }) %>%
    do.call(rbind, .) %>%
    magrittr::set_rownames(NULL)

  trajectory <-
    wrap_data(
      id = id,
      cell_ids = cell_ids
    ) %>%
    add_trajectory(
      milestone_ids = milestone_ids,
      milestone_network = milestone_network,
      progressions = progressions
    ) %>%
    add_dimred(
      dimred = dimred,
      dimred_milestones = dimred_milestones,
      dimred_segment_progressions = dimred_segment_progressions,
      dimred_segment_points = dimred_segment_points
    )
  simp <- simplify_trajectory(trajectory)

  expect_true(all(cell_ids %in% simp$cell_ids))
  expect_true(all(simp$milestone_network$from == c("A", "B", "A", "B", "A")))
  expect_true(all(simp$milestone_network$to == c("B", "C", "E", "F", "C")))

  # check dimred related values
  expect_equal(
    simp$dimred_segment_points %>% as.vector() %>% unique() %>% sort(),
    trajectory$dimred_segment_points %>% as.vector() %>% unique() %>% sort(),
    tol = 1e-3
  )
  expect_equal(nrow(simp$dimred_segment_points), nrow(simp$dimred_segment_progressions))
  expect_lte(nrow(simp$dimred_segment_points), nrow(trajectory$dimred_segment_points))
  expect_equal(simp$dimred_milestones, trajectory$dimred_milestones[simp$milestone_ids, , drop = FALSE])
})
