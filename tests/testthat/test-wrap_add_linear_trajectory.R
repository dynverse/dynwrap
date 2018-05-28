context("Testing add_linear_trajectory")

id <- "a"
cell_ids <- c("truth", "universally", "acknowledged", "that", "a", "single")
extras <- list("man")

pseudotimes <- c(0, .1, .4, .5, .8, 1) %>% set_names(cell_ids)

wr_orig <- wrap_data(
  id = id,
  cell_ids = cell_ids
)

test_that("Testing add_linear_trajectory", {
  wr <-
    wr_orig %>%
    add_linear_trajectory(
      pseudotimes = pseudotimes,
      do_scale_minmax = TRUE,
      directed = FALSE,
      extras = extras
    )

  # testing is_ti_data_wrapper
  expect_true(is_wrapper_with_trajectory(wr))

  expect_equivalent(wr$id, id)
  expect_equivalent(wr$cell_ids, cell_ids)
  expect_equivalent(wr$extras, extras)
  expect_gt(cor(wr$pseudotimes[cell_ids], pseudotimes[cell_ids]), .9)
  expect_equivalent(length(wr$milestone_ids), 2)
  expect_equivalent(nrow(wr$milestone_network), 1)
  expect_equivalent(set_names(sort(unlist(wr$milestone_network[,c("from", "to")])), NULL), sort(wr$milestone_ids))
  expect_true(all(cell_ids %in% wr$progressions$cell_id))
  expect_equivalent(nrow(wr$progressions), length(cell_ids))

  expect_equivalent(wr$trajectory_type, "undirected_linear")
})


test_that("Testing add_linear_trajectory", {
  wr <-
    wr_orig %>%
    add_linear_trajectory(
      pseudotimes = pseudotimes,
      do_scale_minmax = TRUE,
      directed = TRUE,
      extras = extras
    )

  # testing is_ti_data_wrapper
  expect_true(is_wrapper_with_trajectory(wr))

  expect_equivalent(wr$id, id)
  expect_equivalent(wr$cell_ids, cell_ids)
  expect_equivalent(wr$extras, extras)
  expect_gt(cor(wr$pseudotimes[cell_ids], pseudotimes[cell_ids]), .9)
  expect_equivalent(length(wr$milestone_ids), 2)
  expect_equivalent(nrow(wr$milestone_network), 1)
  expect_equivalent(set_names(sort(unlist(wr$milestone_network[,c("from", "to")])), NULL), sort(wr$milestone_ids))
  expect_true(all(cell_ids %in% wr$progressions$cell_id))
  expect_equivalent(nrow(wr$progressions), length(cell_ids))

  expect_equivalent(wr$trajectory_type, "directed_linear")
})


test_that("Testing add_linear_trajectory", {
  wr <-
    wr_orig %>%
    add_linear_trajectory(
      pseudotimes = pseudotimes/10 + .45,
      do_scale_minmax = FALSE,
      directed = TRUE,
      extras = extras
    )

  expect_gt(min(wr$pseudotimes), .4)
  expect_lt(max(wr$pseudotimes), .6)

})

test_that("Testing add_linear_trajectory", {
  wr <-
    wr_orig %>%
    add_linear_trajectory(
      pseudotimes = pseudotimes[-1],
      do_scale_minmax = TRUE,
      directed = TRUE,
      extras = extras
    )

  expect_equivalent(wr$id, id)
  expect_equivalent(wr$cell_ids, cell_ids)
  expect_equivalent(wr$extras, extras)
  expect_equivalent(length(wr$pseudotimes), length(cell_ids) - 1)
  expect_true(all(cell_ids %in% wr$progressions$cell_id))
  expect_equivalent(nrow(wr$progressions), length(cell_ids))
})


test_that("Testing add_linear_trajectory fails when expected", {
  expect_error(
    wr_orig %>%
    add_linear_trajectory(
      pseudotimes = pseudotimes %>% set_names(NULL),
      do_scale_minmax = TRUE,
      directed = FALSE,
      extras = extras
    )
  )
})