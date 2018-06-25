# travis docker not supported https://github.com/travis-ci/travis-ci/issues/5738
if (Sys.getenv("TRAVIS_OS_NAME") != "osx") {
  context("Testing test_docker_installation")

  test_that("Testing that test_docker_installation works", {
    testthat::expect_true(test_docker_installation())
    testthat::expect_message(test_docker_installation(TRUE), "\u2714.*")
  })
}
