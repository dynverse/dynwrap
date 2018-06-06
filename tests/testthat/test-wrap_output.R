context("Testing wrap_output")


test_that("Output processors can process output", {
  cell_ids <- readr::read_csv(
    devtools:::shim_system.file("example_outputs/cell_ids.csv", package="dynwrap"),
    col_types = readr::cols(cell_id=readr::col_character())
  )$cell_id

  base_model <- wrap_data(cell_ids = cell_ids)

  dir_output <- devtools:::shim_system.file("example_outputs", package="dynwrap")

  for (output_format in c("text")) {
    # linear trajectory
    model <- wrap_output(
      base_model,
      "linear_trajectory",
      dir_output,
      output_format
    )
    expect_true(is_wrapper_with_trajectory(model))

    # cyclic trajectory
    model <- wrap_output(
      base_model,
      "cyclic_trajectory",
      dir_output,
      output_format
    )
    expect_true(is_wrapper_with_trajectory(model))

    # pseudotime
    model <- wrap_output(
      base_model,
      "pseudotime",
      dir_output,
      output_format
    )
    expect_true("pseudotime" %in% names(model))

    # grouping
    model <- wrap_output(
      base_model,
      "grouping",
      dir_output,
      output_format
    )
    expect_true(is_wrapper_with_grouping(model))

    # cluster graph
    model <- wrap_output(
      base_model,
      c("grouping", "cluster_graph"),
      dir_output,
      output_format
    )
    expect_true(is_wrapper_with_grouping(model))
    expect_true(is_wrapper_with_trajectory(model))

    # trajectory
    model <- wrap_output(
      base_model,
      c("trajectory"),
      dir_output,
      output_format
    )
    expect_true(is_wrapper_with_trajectory(model))

    # dimred
    model <- wrap_output(
      base_model,
      c("dimred"),
      dir_output,
      output_format
    )
    expect_true(is_wrapper_with_dimred(model))

    # dimred_projection
    model <- wrap_output(
      base_model,
      c("dimred_projection"),
      dir_output,
      output_format
    )
    expect_true(is_wrapper_with_trajectory(model))
  }
})
