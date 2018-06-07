context("Testing wrap_output")


test_that("Make sure that every output has a text example", {
  # get all args which have a specification
  all_args <-
    unique(
      unlist(c(allowed_outputs$required_args, allowed_outputs$optional_args))
    ) %>%
    intersect(names(dynwrap:::output_object_specifications))

  # get all example files
  found_examples <- list.files(devtools:::shim_system.file("example_outputs/text/", package="dynwrap")) %>% tools::file_path_sans_ext()

  expect_true(all(all_args %in% found_examples))
})



cell_ids <- jsonlite::read_json(
  devtools:::shim_system.file("example_outputs/text/cell_ids.json", package="dynwrap"),
) %>% as.character()

base_model <- wrap_data(cell_ids = cell_ids)

for (output_format in c("feather", "rds", "text")) {
  test_that(paste0("Output processors can process output with ", output_format), {
    dir_output <- devtools:::shim_system.file(paste0("example_outputs/", output_format, "/"), package="dynwrap")

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

    # cell_graph
    model <- wrap_output(
      base_model,
      c("cell_graph"),
      dir_output,
      output_format
    )
    expect_true(is_wrapper_with_trajectory(model))

    # cluster_graph
    model <- wrap_output(
      base_model,
      c("cluster_graph"),
      dir_output,
      output_format
    )
    expect_true(is_wrapper_with_trajectory(model))
  })
}


output_format <- "dynwrap"
test_that(paste0("Output processors can process output with ", output_format), {
  dir_output <- devtools:::shim_system.file(paste0("example_outputs/", output_format, "/"), package="dynwrap")

  model <- wrap_output(
    base_model,
    NULL,
    dir_output,
    output_format
  )

  expect_true(is_wrapper_with_trajectory(model))
})
