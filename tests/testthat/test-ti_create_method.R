context("Testing create_ti_method")


test_that("Testing create_ti_method and get_default_parameters with dummy method", {
  dummy <- create_ti_method(
    name = "dummy 1",
    short_name = "dum1",
    package_loaded = c("dynverse"),
    package_required = c("dplyr"),
    par_set = ParamHelpers::makeParamSet(
      ParamHelpers::makeDiscreteParam(id = "param", default = "banana", values = c("apple", "banana", "cherry"))
    ),
    run_fun = function(counts, param = "fjioiw") param,
    plot_fun = function(out) "cake"
  )

  dummy_instance <- dummy()

  expect_equal( dummy_instance$name, "dummy 1" )
  expect_equal( dummy_instance$short_name, "dum1" )
  expect_equal( dummy_instance$package_loaded, "dynverse" )
  expect_equal( dummy_instance$package_required, "dplyr" )
  expect_is( dummy_instance$par_set, "ParamSet" )
  expect_is( dummy_instance$run_fun, "function" )
  # take into account parameter overwriting by parmamset
  expect_equal( dummy_instance$run_fun(NULL), "banana" )
  expect_is( dummy_instance$plot_fun, "function" )
  expect_equal( dummy_instance$plot_fun(NULL), "cake" )

  dummy_instance2 <- dummy(param = "101010")
  expect_equal( dummy_instance2$run_fun(NULL), "101010" )

  expect_equal(get_default_parameters(dummy_instance)$param, "banana")
})
