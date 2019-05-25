context("Checking getTodayCurrencyToEur function")
test_that("check, connection to server and size of currency table", {

  vCurrencyTable <- getTodayCurrencyToEur()
  expect_equal(nrow(vCurrencyTable), 1)

})
