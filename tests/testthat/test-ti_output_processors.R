context("Testing output_processors")


test_that("There is an example for every type of output", {
  output_files <- unique(c(
    unlist(output_processors$required_files),
    unlist(output_processors$optional_files)
  ))

  # print(output_files)

  output_files_found <-
    list.files(devtools:::shim_system.file("example_outputs", package="dynwrap"))

  # print(output_files_found)

  expect_setequal(c(output_files, "cell_ids.csv"), output_files_found)
})


test_that("Output processors can process output", {
  cell_ids <- readr::read_csv(
    devtools:::shim_system.file("example_outputs/cell_ids.csv", package="dynwrap"),
    col_types = readr::cols(cell_id=readr::col_character())
  )$cell_id

  base_model <- wrap_data(cell_ids = cell_ids)

  dir_output <- devtools:::shim_system.file("example_outputs", package="dynwrap")

  # linear
  model <- wrap_output(
    base_model,
    "linear",
    dir_output
  )
  expect_true(is_wrapper_with_trajectory(model))

  # pseudotime
  model <- wrap_output(
    base_model,
    "pseudotime",
    dir_output
  )
  expect_true("pseudotime" %in% names(model))

  # grouping
  model <- wrap_output(
    base_model,
    "grouping",
    dir_output
  )
  expect_true(is_wrapper_with_grouping(model))

  # cluster graph
  model <- wrap_output(
    base_model,
    c("grouping", "cluster_graph"),
    dir_output
  )
  expect_true(is_wrapper_with_grouping(model))
  expect_true(is_wrapper_with_trajectory(model))

  # trajectory
  model <- wrap_output(
    base_model,
    c("trajectory"),
    dir_output
  )
  expect_true(is_wrapper_with_trajectory(model))
})
