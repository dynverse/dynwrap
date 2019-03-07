context("Testing create_ti_method_r")

dummy <- create_ti_method_r(
  name = "dummy 1",
  id = "dum1",
  package_loaded = c("dynwrap"),
  package_required = c("dplyr"),
  input_required = c("counts"),
  input_optional = NULL,
  parameters = dynparam::parameter_set(
    dynparam::character_parameter(
      id = "param",
      default = "banana",
      values = c("apple", "banana", "cherry")
    )
  ),
  run_fun = function(counts, param = "fjioiw", verbose, seed) {
    wrap_data(
      cell_ids = param
    ) %>%
      add_linear_trajectory(
        pseudotime = set_names(0, param)
      )
  }
)

dummy_instance <- dummy()

dataset <-
  wrap_data(cell_ids = "a") %>%
  add_linear_trajectory(pseudotime = c(a = 1)) %>%
  add_expression(
    counts = matrix(0:1, ncol = 2, dimnames = list("a", c("A", "B"))),
    expression = matrix(0:1, ncol = 2, dimnames = list("a", c("A", "B")))
  )

test_that("Testing simple create ti function", {
  expect_equal( dummy_instance$method$name, "dummy 1" )
  expect_equal( dummy_instance$method$id, "dum1" )
  expect_equal( dummy_instance$run$package_loaded, "dynwrap" )
  expect_equal( dummy_instance$run$package_required, "dplyr" )
  expect_is( dummy_instance$parameters, "parameter_set" )
  expect_is( dummy_instance$run$run_fun, "function" )

  expect_equal(get_default_parameters(dummy_instance)$param, "banana")
})

test_that("dummy method is able to correctly pass default argument", {
  expect_equal( infer_trajectory(dataset, dummy_instance)$cell_ids, "banana" )
})

test_that("user is able to override parameter", {
  expect_equal( infer_trajectory(dataset, dummy_instance, parameters = list(param = "cherry"))$cell_ids, "cherry" )
})

test_that("user is able to set different default parameter", {
  dummy_instance2 <- dummy(param = "101010")
  expect_equal( infer_trajectory(dataset, dummy_instance2)$cell_ids, "101010" )


  expect_equal(get_default_parameters(dummy_instance2)$param, "101010")
})

