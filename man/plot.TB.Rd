% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{plot.TB}
\alias{plot.TB}
\title{Plot TB Objects}
\usage{
\method{plot}{TB}(
  x,
  interactive = FALSE,
  conf = FALSE,
  conf.level = 0.95,
  main = "Survival Estimate",
  xlab = "Time",
  ylab = "Survival",
  line.col = "blue",
  showlegend = TRUE,
  main.line = "survival",
  filled = "CI",
  line.width = 2,
  fillcolor = "rgba(128, 128, 128, 0.3)",
  ...
)
}
\arguments{
\item{x}{A TB object.}

\item{conf}{Logical, whether to include confidence bands. Default FALSE.}

\item{conf.level}{Numeric, confidence level for bands. Default 0.95.}

\item{...}{Additional graphical parameters.}
}
\description{
Plot TB Objects
}
\details{
This function generates a static survival plot for TB objects, including optional confidence bands.
}
