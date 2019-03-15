context("Testing wrap_output_list")

# cell data
id <- "a"
cell_ids <- c("truth", "universally", "acknowledged", "that", "a", "single")
cell_info <- data_frame(
  cell_id = cell_ids,
  info1 = c("man", "in", "possession", "of", "a", "good"),
  info2 = c("fortune", "must", "be", "in", "want", "of"),
  info3 = 1:6
)
extras1 <- list("a wife.")
extras2 <- c("However", "little", "known")

# trajectory data
milestone_ids <-  c("man", "in", "possession", "of", "good", "fortune", "must")
milestone_network <- tribble(
  ~from, ~to, ~length, ~directed,
  "man", "in", 1, TRUE,
  "in", "possession", 2, TRUE,
  "in", "of", 3, TRUE,
  "possession", "good", 4, TRUE,
  "of", "fortune", 5, TRUE,
  "good", "must", 6, TRUE,
  "fortune", "must", 7, TRUE
)

# grouping info
grouping <- sample(milestone_ids, length(cell_ids), replace = T) %>% set_names(cell_ids)

# dimred data
num_dims <- round(runif(1, 3, 10))
dim_names <- paste0("comp_", seq_len(num_dims))

dimred <- matrix(runif(num_dims * length(cell_ids), 0, 1), nrow = length(cell_ids), dimnames = list(cell_ids, dim_names))

dimred_milestones <- matrix(runif(num_dims * length(milestone_ids), 0, 1), nrow = length(milestone_ids), dimnames = list(milestone_ids, dim_names))


test_that("wrap_output_list transforms correctly", {
  traj <-
    wrap_data(
      id = id,
      cell_ids = cell_ids,
      cell_info = cell_info
    ) %>%
    add_dimred_projection(
      milestone_ids = milestone_ids,
      milestone_network = milestone_network,
      dimred = dimred,
      dimred_milestones = dimred_milestones,
      grouping = grouping
    )

  trajl <- list(
    id = id,
    cell_ids = cell_ids,
    cell_info = cell_info,
    grouping = grouping,
    milestone_ids = milestone_ids,
    milestone_network = milestone_network,
    dimred = dimred,
    dimred_milestones = dimred_milestones
  )

  traj2 <- wrap_output_list(trajl, c("grouping", "dimred_projection"))

  expect_equal(sort(names(traj2)), sort(names(traj)))
  for (n in names(traj2)) {
    expect_equal(traj2[[n]], traj[[n]], info = n)
  }
})

