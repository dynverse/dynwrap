context("Testing infer_trajectory")

# create task
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

num_features <- round(runif(1, 100, 120))
feature_names <- paste0("feature_", seq_len(num_features))

expression <- matrix(runif(num_features * length(cell_ids), 8, 12), nrow = length(cell_ids), dimnames = list(cell_ids, feature_names))
counts <- 2^expression - 1
feature_info <- data_frame(feature_id = feature_names, mean = colMeans(expression), var = apply(expression, 2, var))

task <-
  wrap_expression(
    id = id,
    expression,
    counts,
    cell_info,
    feature_info,
    extras1 = extras1,
    extras2 = extras2
  ) %>%
  add_prior_information(start_id = cell_ids[[1]])

test_that("Testing infer_trajectory with control methods", {
  method <- ti_comp1()

  model <- infer_trajectory(task, method)
  expect_s3_class(model, "dynwrap::with_trajectory")

  # test plotting
  expect_true("ggplot" %in% class(method$plot_fun(model)))

  # test priors
  model <- infer_trajectory(task, method, give_priors = c("start_id"))
  expect_s3_class(model, "dynwrap::with_trajectory")

  expect_error(infer_trajectory(task, method, give_priors = c("to be or not to be")))

  # run with multiple tasks and one method
  models <- infer_trajectories(list(task, task), method)
  expect_true(is_tibble(models))
  expect_equal(nrow(models), 2)
  expect_setequal(c("task_ix", "method_ix", "model", "method_name", "task_id", "summary"), names(models))

  models <- infer_trajectories(list_as_tibble(list(task, task)), ti_comp1())
  expect_true(is_tibble(models))
  expect_equal(nrow(models), 2)

  # run with multiple methods
  models <- infer_trajectories(task, list(ti_comp1(), ti_comp1()))
  expect_true(is_tibble(models))
  expect_equal(nrow(models), 2)

  models <- infer_trajectories(task, list_as_tibble(list(ti_comp1(), ti_comp1())))
  expect_true(is_tibble(models))
  expect_equal(nrow(models), 2)

  models <- infer_trajectories(task, c("comp1", "comp1"))
  expect_true(is_tibble(models))
  expect_equal(nrow(models), 2)

  expect_message(infer_trajectories(task, c("camp1")))

  expect_error(infer_trajectories(task, c(1,2,3)))
  expect_error(infer_trajectories(c(1,2,3), c(1,2,3)))

  # run with multiple tasks and multiple methods
  models <- infer_trajectories(
    task = list(task, task, task),
    method = list(ti_comp1(), ti_comp1())
  )

  expect_true(is_tibble(models))
  expect_equal(nrow(models), 6)

  # run with multiple tasks and multiple methods with specified parameters
  models <- infer_trajectories(
    task = list(task, task),
    method = list_as_tibble(list(ti_comp1(), ti_comp1())),
    parameters = list(list(method = "mds"), list(method = "pca"))
  )

  expect_true(is_tibble(models))
  expect_equal(nrow(models), 4)
})



test_that("Testing ti_comp1", {
  method <- ti_comp1()
  model <- method$run_fun(task$expression)

  plot <- method$plot_fun(model)
  expect_is(plot, "ggplot")
})
