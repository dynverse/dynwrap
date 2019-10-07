context("Testing add_timings")

test_that("Testing add_timings", {
  id <- "a"
  cell_ids <- c("truth", "universally", "acknowledged", "that", "a", "single")
  cell_info <- tibble(
    cell_id = cell_ids,
    info1 = c("man", "in", "possession", "of", "a", "good"),
    info2 = c("fortune", "must", "be", "in", "want", "of"),
    info3 = 1:6
  )
  extras1 <- list("a wife.")
  extras2 <- c("However", "little", "known")

  tl <- add_timing_checkpoint(NULL, "start")
  Sys.sleep(1)
  tl <- tl %>% add_timing_checkpoint("second")
  Sys.sleep(.5)
  tl <- tl %>% add_timing_checkpoint("third")
  Sys.sleep(.25)
  tl <- tl %>% add_timing_checkpoint("stop")

  wr <-
    wrap_data(
      id = id,
      cell_ids = cell_ids,
      cell_info = cell_info,
      extras1 = extras1,
      extras2 = extras2
    ) %>%
    add_timings(
      timings = tl
    )

  # testing is_ti_data_wrapper
  expect_true(is_wrapper_with_timings(wr))
  expect_false(is_wrapper_with_timings(list(chvehoie = "jihofrewghifu")))

  testthat::expect_equivalent(wr$timings, tl)
  testthat::expect_equivalent(names(tl), c("start", "second", "third", "stop"))

  diffs <- diff(unlist(wr$timings))
  testthat::expect_true(all(abs(diffs - c(1, .5, .25)) < .1))
})
