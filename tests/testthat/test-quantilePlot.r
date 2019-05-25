context("Checking cleanDirectory function")
test_that("check quantile plot creation", {


  data <- data.frame(iris)
  vPlot <- quantilePlot(data, 0.3, 0.95)
  # plotly objects are lists, so correctly created plot
  # should have length bigger than 1
  expect_gt(length(vPlot), 1)

})


