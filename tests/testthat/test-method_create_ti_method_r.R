context("Testing create_ti_method_r")

dummy_definition <- definition(
  method = def_method(
    id = "dummy"
  ),
  parameters = dynparam::parameter_set(
    dynparam::character_parameter(
      id = "fruit",
      default = "banana",
      values = c("apple", "banana", "cherry")
    )
  ),
  wrapper = def_wrapper(
    input_required = "expression",
    input_optional = "start_id"
  )
)

dummy_run_fun = function(counts, parameters) {
  wrap_data(
    cell_ids = parameters$fruit
  ) %>%
    add_linear_trajectory(
      pseudotime = set_names(0, parameters$fruit)
    )
}


dummy <- create_ti_method_r(
  dummy_definition,
  dummy_run_fun,
  package_loaded = "dynwrap",
  package_required = "dplyr",
  return_function = TRUE
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
  expect_equal( dummy_instance$method$name, "dummy" )
  expect_equal( dummy_instance$method$id, "dummy" )
  expect_equal( dummy_instance$run$package_loaded, "dynwrap" )
  expect_equal( dummy_instance$run$package_required, "dplyr" )
  expect_is( dummy_instance$parameters, "parameter_set" )
  expect_is( dummy_instance$run$run_fun, "function" )

  expect_equal(get_default_parameters(dummy_instance)$fruit, "banana")
})

test_that("dummy method is able to correctly pass default argument", {
  expect_equal( infer_trajectory(dataset, dummy_instance)$cell_ids, "banana" )
})

test_that("user is able to override parameter", {
  expect_equal( infer_trajectory(dataset, dummy_instance, parameters = list(fruit = "cherry"))$cell_ids, "cherry" )
})

test_that("user is able to set different default parameter", {
  dummy_instance2 <- dummy(fruit = "101010")
  expect_equal( infer_trajectory(dataset, dummy_instance2)$cell_ids, "101010" )

  expect_equal(get_default_parameters(dummy_instance2)$fruit, "101010")
})

