context("Checking linearModel function")
test_that("check, if number of coefficients is equal to number of independent variables", {

  data = data.frame(y = c(1:100), x1 = runif(100), x2 = c(2:101) + runif(100))
  vCoeffs = linearModel(y ~ x1 + x2, data = data) # TODO test would fail if we take subset of variables to model from data
  expect_equal(length(vCoeffs), ncol(data) - 1)

})
