context("Testing wrap_output")

test_that("Make sure that every output has a text example", {
  # get all args which have a specification
  all_args <-
    unique(
      unlist(c(allowed_outputs$required_args, allowed_outputs$optional_args))
    ) %>%
    intersect(names(dynwrap:::output_object_specifications))

  # get all example files
  found_examples <- list.files(devtools:::shim_system.file("example_outputs/text/", package = "dynwrap")) %>% tools::file_path_sans_ext()

  expect_true(all(all_args %in% found_examples))
})

