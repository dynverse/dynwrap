context("Testing safe_tempdir")

test_that("safe_tempdir works correctly", {
  dir1 <- safe_tempdir("mydir")
  dir2 <- safe_tempdir("mydir")
  on.exit(unlink(dir1, recursive = TRUE))
  on.exit(unlink(dir2, recursive = TRUE))

  expect_true(dir1 != dir2)
  expect_true(dir.exists(dir1))
  expect_true(dir.exists(dir2))
})
