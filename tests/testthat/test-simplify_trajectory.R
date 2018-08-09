context("Testing simplify_trajectory")

test_that("Simple test", {
  id <- "a"
  cell_ids <- c("truth", "universally", "acknowledged", "that", "a", "single")
  extras <- list("man")

  milestone_ids <- c("must", "be", "in", "want")
  milestone_network <- data_frame(
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

  traj <-
    wrap_data(
      id = id,
      cell_ids = cell_ids
    ) %>%
    add_trajectory(
      milestone_ids = milestone_ids,
      milestone_network = milestone_network,
      progressions = progressions
    )
  simp <- simplify_trajectory(traj)

  #' @examples
  #' dynplot::plot_graph(traj)
  #' dynplot::plot_graph(simp)


})
