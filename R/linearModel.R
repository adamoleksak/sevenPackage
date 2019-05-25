#' @title  Draws Coefficients Barplot
#' @description linearModel is used to vizualize coefficients of linear models as barplot.
#'     It can be used for any linear model equation.
#' @param formula Linear model formula
#' @param data data to model
#' @return barplot with linear model coefficients without intercept
#' @export
#' @importFrom stats quantile
#' @importFrom stats lm
#' @importFrom graphics barplot
#' @importFrom graphics plot
#' @examples
#' data = data.frame(y = c(1:100), x1 = runif(100), x2 = c(2:101)+runif(100))
#' linearModel(y~ x1+x2, data = data)

linearModel <- function(formula, data){
  vModel <- lm(formula, data = data)
  vModelCoefficients <- t(data.frame(vModel$coefficients))
  assert_that(ncol(vModelCoefficients) > 2, msg = "Coefficients are not calculated correctly, check number of columns in data")
  # drop intercept value, it is in first columns:
  vModelCoefficients <- vModelCoefficients[, -1]
  barplot(vModelCoefficients)
}
