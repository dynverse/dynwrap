context("Testing .wrap_text_reader")

test_that(".wrap_text_reader fails gracefully", {
  expect_error(.wrap_text_reader("/ti/output/I_like_to_eat_eat_eat_apples_and_bananas.csv"), "No specification for")
})
