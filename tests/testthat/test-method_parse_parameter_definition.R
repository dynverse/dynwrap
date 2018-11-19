# context("Testing parse_parameter_defintion")
#
# parameter_definition <- list(
#   fixed = list(
#     type = "integer",
#     default = 42
#   ),
#   integer = list(
#     type = "integer",
#     lower = 10,
#     upper = 20,
#     default = 15
#   ),
#   integer_norm = list(
#     type = "integer",
#     lower = 10,
#     upper = 20,
#     default = 15,
#     mean = 15,
#     sd = 0.5,
#     distribution = "normal"
#   ),
#   integer_exp = list(
#     type = "integer",
#     lower = 10,
#     upper = 20,
#     default = 15,
#     rate = 1,
#     distribution = "exponential"
#   ),
#   integer_expunif = list(
#     type = "integer",
#     lower = 10,
#     upper = 20,
#     default = 15,
#     distribution = "expuniform"
#   ),
#   numeric = list(
#     type = "numeric",
#     lower = 10,
#     upper = 20,
#     default = 15
#   ),
#   numeric_norm = list(
#     type = "numeric",
#     lower = 10,
#     upper = 20,
#     default = 15,
#     mean = 15,
#     sd = 0.5,
#     distribution = "normal"
#   ),
#   numeric_exp = list(
#     type = "numeric",
#     lower = 10,
#     upper = 20,
#     default = 15,
#     rate = 1,
#     distribution = "exponential"
#   ),
#   numeric_expunif = list(
#     type = "numeric",
#     lower = 10,
#     upper = 20,
#     default = 15,
#     distribution = "expuniform"
#   ),
#   integer_vector = list(
#     type = "integer_vector",
#     lower = 10,
#     upper = 20,
#     default = c(15, 10, 11, 12, 18),
#     length = 5
#   ),
#   numeric_vector = list(
#     type = "numeric_vector",
#     lower = 10,
#     upper = 20,
#     default = c(15, 10, 11, 12, 18),
#     length = 5
#   ),
#   discrete = list(
#     type = "discrete",
#     values = c("a", "b", "c"),
#     default = "a"
#   ),
#   discrete_vector = list(
#     type = "discrete_vector",
#     values = c("a", "b", "c"),
#     default = c("a", "b"),
#     length = 2
#   ),
#   logical = list(
#     type = "logical",
#     default = TRUE
#   ),
#   logical_vector = list(
#     type = "logical_vector",
#     length = 2,
#     default = c(TRUE, FALSE)
#   )
# )
#
# par_set <- parse_parameter_definition(parameter_definition)
#
# sampled_parameters <- ParamHelpers::generateDesign(1000, par_set, trafo = TRUE) %>%
#   ParamHelpers::dfRowsToList(par_set) %>%
#   dynutils::list_as_tibble()
#
# test_that("Parameters can be parsed and sampled", {
#   testthat::expect_setequal(sampled_parameters$fixed, 42)
#
#   numbers <- sampled_parameters[, c("integer", "numeric", "integer_vector", "numeric_vector")] %>% unlist()
#   testthat::expect_true(all(numbers >= 10))
#   testthat::expect_true(all(numbers <= 20))
#
#   discretes <- sampled_parameters[, c("discrete", "discrete_vector")] %>% unlist()
#   testthat::expect_true(all(discretes %in% c("a", "b", "c")))
#
#   logicals <- sampled_parameters[, c("logical", "logical_vector")] %>% unlist()
#   testthat::expect_true(all(logicals %in% c(TRUE, FALSE)))
# })
#
#
#
# test_that("Parameters are sampled from the correct distributions", {
#   for(col in c("numeric", "numeric_vector")) {
#     numbers <- sampled_parameters[,col ] %>% unlist()
#
#     expect_gt(ks.test(numbers, runif(10000, 10, 20))$p.value, 0.1)
#   }
#
#   for(col in c("integer",  "integer_vector")) {
#     numbers <- sampled_parameters[,col ] %>% unlist()
#
#     suppressWarnings(
#       expect_gt(ks.test(numbers, round(runif(10000, 10, 20)))$p.value, 0.1)
#     )
#   }
#
#   col <- "numeric_norm"
#   numbers <- sampled_parameters[,col ] %>% unlist()
#   expect_gt(ks.test(numbers, rnorm(10000, 15, 0.5))$p.value, 0.1)
#
#   col <- "integer_norm"
#   numbers <- sampled_parameters[,col ] %>% unlist()
#   suppressWarnings(
#     expect_gt(ks.test(numbers, round(rnorm(10000, 15, 0.5)))$p.value, 0.1)
#   )
#
#   col <- "numeric_exp"
#   numbers <- sampled_parameters[,col ] %>% unlist()
#   numbers2 <- rexp(100000, 1) + 10
#   numbers2 <- numbers2[numbers2 < 20]
#   suppressWarnings(
#     expect_gt(ks.test(numbers, numbers2)$p.value, 0.1)
#   )
#
#   col <- "integer_exp"
#   numbers <- sampled_parameters[,col ] %>% unlist()
#   numbers2 <- rexp(100000, 1) + 10
#   numbers2 <- round(numbers2[numbers2 < 20])
#   suppressWarnings(
#     expect_gt(ks.test(numbers, numbers2)$p.value, 0.1)
#   )
# })
