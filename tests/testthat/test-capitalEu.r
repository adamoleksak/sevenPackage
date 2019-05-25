context("Checking capitalEu function")
test_that("check, capitalEu", {

  vRes <- capitalEu("POLAND", "lower")

  expect_equal(vRes, "warsaw")

})
