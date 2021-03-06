% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/propTest.R
\name{propTest}
\alias{propTest}
\title{propTest}
\usage{
propTest(data, xOrder = NULL, alternative = "two.sided", success = NULL,
  conf = 0.95, alpha = 0.05)
}
\arguments{
\item{data}{data frame containing data to be analyzed, with the response variable as the first variable.}

\item{xOrder}{when x is given, order of levels of x in which to subtract parameters}

\item{alternative}{direction of the alternative hypothesis; "less","greater", or "two.sided"}

\item{conf}{confidence level, value between 0 and 1}

\item{alpha}{numeric between 0 and 1, the probability of a type I error}
}
\value{
Data frame containing the results of the z-test
}
\description{
\code{propTest} Conducts a z-tests for a multi-level categorical explanatory
variable on a response variable. Returns data and plots of the z-distribution
vis-a-vis the observed test statistic.
}
\seealso{
Other xmar functions: \code{\link{X23D}}, \code{\link{X2}},
  \code{\link{analyze3d}}, \code{\link{analyze}},
  \code{\link{discussion}}, \code{\link{formatData}},
  \code{\link{plotBars}}, \code{\link{preprocess}},
  \code{\link{univariate}}, \code{\link{zTest}}
}
\author{
John James, \email{jjames@datasciencesalon.org}
}
