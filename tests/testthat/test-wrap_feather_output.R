context("Testing wrap_feather")

output_format <- "feather"
inst_dir <- devtools:::shim_system.file(paste0("example_outputs/", output_format, "/"), package = "dynwrap")

test_trajectory_type <- function(output_ids, files, model_test_fun) {
  dir_output <- tempfile()
  dir.create(dir_output)
  on.exit(unlink(dir_output, recursive = TRUE))
  file.copy(paste0(inst_dir, "/", files), dir_output)

  test_that(paste0("wrap_", output_format, " can process [", paste0(output_ids, collapse = ", "), "]"), {
    model <- wrap_output(output_ids, dir_output, output_format)
    model_test_fun(model)
  })
}

test_trajectory_type(
  output_ids = "linear_trajectory",
  files = c("cell_ids.feather", "pseudotime.feather"),
  model_test_fun = function(model) {
    expect_true(is_wrapper_with_trajectory(model))
  }
)

test_trajectory_type(
  "cyclic_trajectory",
  c("cell_ids.feather", "pseudotime.feather"),
  function(model) {
    expect_true(is_wrapper_with_trajectory(model))
  }
)

test_trajectory_type(
  "pseudotime",
  c("cell_ids.feather", "pseudotime.feather"),
  function(model) {
    expect_true("pseudotime" %in% names(model))
  }
)

test_trajectory_type(
  "grouping",
  c("cell_ids.feather", "group_ids.feather", "grouping.feather"),
  function(model) {
    expect_true(is_wrapper_with_grouping(model))
  }
)

# without group ids
test_trajectory_type(
  "grouping",
  c("cell_ids.feather", "grouping.feather"),
  function(model) {
    expect_true(is_wrapper_with_grouping(model))
  }
)

test_trajectory_type(
  c("cluster_graph"),
  c("cell_ids.feather", "milestone_network.feather", "grouping.feather"),
  function(model) {
    expect_true(is_wrapper_with_grouping(model))
    expect_true(is_wrapper_with_trajectory(model))
  }
)

# trajectory with progressions
test_trajectory_type(
  c("trajectory"),
  c("cell_ids.feather", "milestone_network.feather", "progressions.feather"),
  function(model) {
    expect_true(is_wrapper_with_trajectory(model))
  }
)

# trajectory with milestone percentages
test_trajectory_type(
  c("trajectory"),
  c("cell_ids.feather", "milestone_ids.feather", "milestone_network.feather", "milestone_percentages.feather"),
  function(model) {
    expect_true(is_wrapper_with_trajectory(model))
  }
)

test_trajectory_type(
  c("branch_trajectory"),
  c("cell_ids.feather", "branch_network.feather", "branches.feather", "branch_progressions.feather"),
  function(model) {
    expect_true(is_wrapper_with_trajectory(model))
  }
)

test_trajectory_type(
  c("dimred"),
  c("cell_ids.feather", "dimred.feather", "dimred_milestones.feather"),
  function(model) {
    expect_true(is_wrapper_with_dimred(model))
  }
)

test_trajectory_type(
  c("dimred_projection"),
  c("cell_ids.feather", "milestone_ids.feather", "milestone_network.feather", "dimred.feather", "dimred_milestones.feather", "grouping.feather"),
  function(model) {
    expect_true(is_wrapper_with_dimred(model))
    expect_true(is_wrapper_with_trajectory(model))
  }
)

test_trajectory_type(
  output_ids = c("cell_graph"),
  files = c("cell_ids.feather", "cell_graph.feather", "to_keep.feather"),
  model_test_fun = function(model) {
    expect_true(is_wrapper_with_trajectory(model))
  }
)
