context("Checking cleanDirectory function")
test_that("check deletion of csv files", {

  vGetWd <- getwd()
  # create csv file
  system("touch file.csv")
  cleanDirectory(vGetWd)
  vCsvFiles = list.files(path = vGetWd, pattern = "*.csv")
  expect_equal(length(vCsvFiles), 0)

})
