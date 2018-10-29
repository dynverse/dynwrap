context("Testing wrap_rds")

output_format <- "rds"
inst_dir <- pkgload:::shim_system.file(paste0("example_outputs/", output_format, "/"), package = "dynwrap")

test_trajectory_type <- function(output_ids, files, model_test_fun) {
  dir_output <- tempfile()
  dir.create(dir_output)
  on.exit(unlink(dir_output, recursive = TRUE))
  model <- readr::read_rds(paste0(inst_dir, "/output.rds"))
  model <- model[files]
  readr::write_rds(model, paste0(dir_output, "/output.rds"))

  test_that(paste0("wrap_", output_format, " can process [", paste0(output_ids, collapse = ", "), "]"), {
    model <- wrap_output(output_ids, dir_output, output_format)
    model_test_fun(model)
  })
}

test_trajectory_type(
  output_ids = "linear_trajectory",
  files = c("cell_ids", "pseudotime"),
  model_test_fun = function(model) {
    expect_true(is_wrapper_with_trajectory(model))
  }
)

test_trajectory_type(
  "cyclic_trajectory",
  c("cell_ids", "pseudotime"),
  function(model) {
    expect_true(is_wrapper_with_trajectory(model))
  }
)

test_trajectory_type(
  "pseudotime",
  c("cell_ids", "pseudotime"),
  function(model) {
    expect_true("pseudotime" %in% names(model))
  }
)

test_trajectory_type(
  "grouping",
  c("cell_ids", "group_ids", "grouping"),
  function(model) {
    expect_true(is_wrapper_with_grouping(model))
  }
)

# without group ids
test_trajectory_type(
  "grouping",
  c("cell_ids", "grouping"),
  function(model) {
    expect_true(is_wrapper_with_grouping(model))
  }
)

test_trajectory_type(
  c("cluster_graph"),
  c("cell_ids", "milestone_network", "grouping"),
  function(model) {
    expect_true(is_wrapper_with_grouping(model))
    expect_true(is_wrapper_with_trajectory(model))
  }
)

# trajectory with progressions
test_trajectory_type(
  c("trajectory"),
  c("cell_ids", "milestone_network", "progressions"),
  function(model) {
    expect_true(is_wrapper_with_trajectory(model))
  }
)

# trajectory with milestone percentages
test_trajectory_type(
  c("trajectory"),
  c("cell_ids", "milestone_ids", "milestone_network", "milestone_percentages"),
  function(model) {
    expect_true(is_wrapper_with_trajectory(model))
  }
)

test_trajectory_type(
  c("branch_trajectory"),
  c("cell_ids", "branch_network", "branches", "branch_progressions"),
  function(model) {
    expect_true(is_wrapper_with_trajectory(model))
  }
)

test_trajectory_type(
  c("dimred"),
  c("cell_ids", "dimred", "dimred_milestones"),
  function(model) {
    expect_true(is_wrapper_with_dimred(model))
  }
)

test_trajectory_type(
  c("dimred_projection"),
  c("cell_ids", "milestone_ids", "milestone_network", "dimred", "dimred_milestones", "grouping"),
  function(model) {
    expect_true(is_wrapper_with_dimred(model))
    expect_true(is_wrapper_with_trajectory(model))
  }
)

test_trajectory_type(
  c("timings"),
  c("cell_ids", "timings"),
  function(model) {
    expect_true(is_wrapper_with_timings(model))
  }
)

test_trajectory_type(
  output_ids = c("cell_graph"),
  files = c("cell_ids", "cell_graph", "to_keep"),
  model_test_fun = function(model) {
    expect_true(is_wrapper_with_trajectory(model))
  }
)
