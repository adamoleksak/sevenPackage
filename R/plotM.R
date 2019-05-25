#' @title  Simple Plot Manipulation
#' @description plotM can be used to manipulate ranges, styles and
#'    colors of displayed data
#' @param data dataframe to plot
#' @param col1 x column name
#' @param col2 y column name
#' @return Plot which can be manipulated
#' @export
#' @importFrom manipulate manipulate
#' @importFrom manipulate slider
#' @importFrom manipulate picker
#' @importFrom graphics plot
#' @importFrom grDevices colors

plotM <- function(data, col1, col2){
  vData <- data[, c(col1, col2)]
  vHalf <- max(vData[, col1], na.rm = T)/2
  vMin <- min(vData[, col2], na.rm = T)
  if(ncol(vData) != 2) stop("Data to plot has more or less than 2 columns")
  if(vHalf < vMin) stop("Comparison of max value and min value in data is wrong")
  manipulate(
    plot(vData,
         xlim = c(xMin, xMax),
         pch = as.numeric(pch),
         col = colors,
         main = "Simple manipulation plot"),
    xMin = slider(vMin, vHalf),
    xMax = slider(vHalf + vMin, vHalf * 2),
    pch = picker("1", "2", "3", "4", "5", "16"),
    colors = picker("red", "green", "yellow")
  )
}

