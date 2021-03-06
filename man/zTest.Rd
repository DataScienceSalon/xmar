% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zTest.R
\name{zTest}
\alias{zTest}
\title{zTest}
\usage{
zTest(data, success = "Traditional", scope, alternative = "two.sided",
  conf = 0.95, alpha = 0.05)
}
\arguments{
\item{data}{Explanatory and response variable values}

\item{success}{Which level of response variable to call "success", i.e. upon which inference should be done. Defaults to order in which factor variables are defined.}

\item{scope}{Character string indicating the scope of the data being analyzed.}

\item{alternative}{direction of the alternative hypothesis; "less","greater", or "two.sided"}

\item{conf}{Desired confidence level}

\item{alpha}{Probability of a type I error}
}
\value{
Data frame containing the results of the z-test
}
\description{
\code{zTest} Conducts a z-tests difference in proportions for a dichotomous
categorical explanatory variable on a response variable. Assumes the null
hypothesis is equal proportions.
}
\seealso{
Other xmar functions: \code{\link{X23D}}, \code{\link{X2}},
  \code{\link{analyze3d}}, \code{\link{analyze}},
  \code{\link{discussion}}, \code{\link{formatData}},
  \code{\link{plotBars}}, \code{\link{preprocess}},
  \code{\link{propTest}}, \code{\link{univariate}}
}
\author{
John James, \email{jjames@datasciencesalon.org}
}
