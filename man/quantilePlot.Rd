% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quantilePlot.R
\name{quantilePlot}
\alias{quantilePlot}
\title{Plot Numeric Variables as HeatMap}
\usage{
quantilePlot(data, q1, q2)
}
\arguments{
\item{data}{data table to be plotted}

\item{q1}{quantile below which values are not plotted}

\item{q2}{quantile above which values are not plotted}
}
\value{
returns plot with selected values
}
\description{
quantilePlot is used to vizualize numeric columns from table data in form of heatmap.
    It is also possible to exclude outliers by specifying quantiles
    For example, if q1 = 0.2 and q2 = 0.8, it means that heatmap would show
    values between q1 and q2
}
\examples{
data <- data.frame(iris)
quantilePlot(data, 0.3, 0.95)
}
