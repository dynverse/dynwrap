context("Testing output_processors")


test_that("There is an example for every type of input", {
  input_files <- unique(input_processors$file)

  input_files_found <-
    list.files(devtools:::shim_system.file("example_inputs", package="dynwrap"))

  expect_setequal(input_files, input_files_found)
})
