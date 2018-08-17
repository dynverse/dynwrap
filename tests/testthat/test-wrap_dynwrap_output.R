context("Testing wrap_dynwrap")


# simple test
output_format <- "dynwrap"
test_that(paste0("Output processors can process output with ", output_format), {
  dir_output <- devtools:::shim_system.file(paste0("example_outputs/", output_format, "/"), package = "dynwrap")

  model <- wrap_output(
    NULL,
    dir_output,
    output_format
  )

  expect_true(is_wrapper_with_trajectory(model))
})

# test by rewrapping text output
output_format <- "dynwrap"
inst_dir <- devtools:::shim_system.file(paste0("example_outputs/text/"), package = "dynwrap")

test_trajectory_type <- function(output_ids, files, model_test_fun) {
  dir_output <- tempfile()
  dir.create(dir_output)
  on.exit(unlink(dir_output, recursive = TRUE))
  file.copy(paste0(inst_dir, "/", files), dir_output)

  test_that(paste0("wrap_", output_format, " can process [", paste0(output_ids, collapse = ", "), "]"), {
    model <- wrap_output(output_ids, dir_output, output_format = "text")
    readr::write_rds(model, paste0(dir_output, "/output.rds"))
    wrap_output(output_ids %>% str_replace("\\.[^\\.]*", ""), dir_output, output_format)
    model_test_fun(model)
  })
}

test_trajectory_type(
  "linear_trajectory",
  c("cell_ids.csv", "pseudotime.csv"),
  function(model) {
    expect_true(is_wrapper_with_trajectory(model))
  }
)

test_trajectory_type(
  "cyclic_trajectory",
  c("cell_ids.csv", "pseudotime.csv"),
  function(model) {
    expect_true(is_wrapper_with_trajectory(model))
  }
)

test_trajectory_type(
  "pseudotime",
  c("cell_ids.csv", "pseudotime.csv"),
  function(model) {
    expect_true("pseudotime" %in% names(model))
  }
)

test_trajectory_type(
  "grouping",
  c("cell_ids.csv", "group_ids.json", "grouping.csv"),
  function(model) {
    expect_true(is_wrapper_with_grouping(model))
  }
)

# without group ids
test_trajectory_type(
  "grouping",
  c("cell_ids.csv", "grouping.csv"),
  function(model) {
    expect_true(is_wrapper_with_grouping(model))
  }
)

test_trajectory_type(
  c("cluster_graph"),
  c("cell_ids.csv", "milestone_network.csv", "grouping.csv"),
  function(model) {
    expect_true(is_wrapper_with_grouping(model))
    expect_true(is_wrapper_with_trajectory(model))
  }
)

# trajectory with progressions
test_trajectory_type(
  c("trajectory"),
  c("cell_ids.csv", "milestone_network.csv", "progressions.csv"),
  function(model) {
    expect_true(is_wrapper_with_trajectory(model))
  }
)

# trajectory with milestone percentages
test_trajectory_type(
  c("trajectory"),
  c("cell_ids.csv", "milestone_ids.json", "milestone_network.csv", "milestone_percentages.csv"),
  function(model) {
    expect_true(is_wrapper_with_trajectory(model))
  }
)

test_trajectory_type(
  output_ids = c("branch_trajectory"),
  files = c("cell_ids.csv", "branch_network.csv", "branches.csv", "branch_progressions.csv"),
  model_test_fun = function(model) {
    expect_true(is_wrapper_with_trajectory(model))
  }
)

test_trajectory_type(
  c("dimred"),
  c("cell_ids.csv", "dimred.csv", "dimred_milestones.csv"),
  function(model) {
    expect_true(is_wrapper_with_dimred(model))
  }
)

test_trajectory_type(
  c("dimred_projection"),
  c("cell_ids.csv", "milestone_ids.json", "milestone_network.csv", "dimred.csv", "dimred_milestones.csv", "grouping.csv"),
  function(model) {
    expect_true(is_wrapper_with_dimred(model))
    expect_true(is_wrapper_with_trajectory(model))
  }
)

test_trajectory_type(
  output_ids = c("cell_graph"),
  files = c("cell_ids.csv", "cell_graph.csv", "to_keep.json"),
  model_test_fun = function(model) {
    expect_true(is_wrapper_with_trajectory(model))
  }
)
