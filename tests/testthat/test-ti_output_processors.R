context("Testing output_processors")


test_that("There is an example for every type of output", {
  output_files <- unique(unlist(output_processors$required_files))

  output_files_found <-
    list.files(devtools:::shim_system.file("example_outputs", package="dynwrap"))

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
})
