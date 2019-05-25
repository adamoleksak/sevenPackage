getQuantile <- function(column, q1, q2){
  vVector <- as.vector(column)
  vQuantiles <- quantile(vVector, prob = c(q1, q2))
  vVector <- ifelse(between(vVector, vQuantiles[1], vQuantiles[2]), vVector, NA)
  vVector
}

#' @title  Plot Numeric Variables as HeatMap
#' @description quantilePlot is used to vizualize numeric columns from table data in form of heatmap.
#'     It is also possible to exclude outliers by specifying quantiles
#'     For example, if q1 = 0.2 and q2 = 0.8, it means that heatmap would show
#'     values between q1 and q2
#' @param data data table to be plotted
#' @param q1 quantile below which values are not plotted
#' @param q2 quantile above which values are not plotted
#' @return returns plot with selected values
#' @export
#' @importFrom plotly plot_ly
#' @importFrom data.table data.table
#' @importFrom data.table between
#' @importFrom stats quantile
#' @examples
#' data <- data.frame(iris)
#' quantilePlot(data, 0.3, 0.95)

quantilePlot <- function(data, q1, q2){
  assert_that(q1 < q2, msg = "q1 must be lower than q2")
  assert_that(nrow(data) > 0, msg = "data must have more than 0 rows")

  vData <- data.table(data)
  vNumericCols <- unlist(lapply(vData, is.numeric))
  assert_that(length(vNumericCols) > 1, msg = "There is no numeric variables in given data")

  vData <- vData[, vNumericCols, with = F]
  vSDCols <- colnames(vData)
  vData <- vData[, lapply(.SD, function(x) getQuantile(x, q1, q2)), .SDcols = vSDCols]
  p <- plot_ly(z = as.matrix(vData), type = "heatmap", colors = "YlOrRd")
  p #' TODO add x axis, title
}





