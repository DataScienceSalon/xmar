#==============================================================================#
#                                     analyze                                  #
#==============================================================================#
#' analyze
#'
#' \code{results} Performs an analysis for a data frame containing a response
#' and an explanatory variable.  The key functions are:
#' \describe{
#'  \item{formatData(d, y, x)}{Takes three variables: d, a data frame containing response and explanatory variable;
#'  y, a label for the response variable; and x, a label for the explanatory variable, and returns the original raw data
#'  as well as the summarized observed and expected frequencies.}
#'  \item{x2(d, y, x, conf, alpha)}{Takes four variables: d, a data frame containing response and explanatory variable;,
#'  y, a label for the response variable; x, a label for the explanatory variable; conf numeric between 0 and 1 that indicates
#'  the confidence level; and alpha a number between 0 and 1 that indicates the probability of a type I error. It returns the
#'  results of a chi-square test of independence with plot and descriptive statements regarding the result.}
#'  \item{zTest(d, success, order, alternative, conf, alpha)}{Takes six variables: d, a data frame containing response and explanatory variable;
#'  success is the level of the response variable considered for analysis; order is the order of the levels for the explanatory variable;,
#'  alternative is the alternative hypothesis for the test; conf numeric between 0 and 1 that indicates
#'  the confidence level; and alpha a number between 0 and 1 that indicates the probability of a type I error. It returns the results
#'  of a two-proportion z-test with descriptive statements.}
#'  \item{propTest(d, order, alternative, success, conf, alpha)}{Takes six variables: d, a data frame containing response and explanatory variable;
#'  success is the level of the response variable considered for analysis; order is the order of the levels for the explanatory variable;,
#'  alternative is the alternative hypothesis for the test; conf numeric between 0 and 1 that indicates
#'  the confidence level; and alpha a number between 0 and 1 that indicates the probability of a type I error. It returns the results
#'  of a two-proportion z-test with descriptive statements.}
#' }
#'
#' @param data Data frame containing two columns; the response and the explanatory variable.
#' @param y Character string label for the response variable.
#' @param x Character string label for the explanatory variable.
#' @param title Character string containing title of the plot
#' @param success Character string equivalent to the response variable level to be analyzed.
#' @param scope Character string indicating the scope of the data.
#' @param conf Numeric between 0 and 1, indicates desired confidence level for hypothesis test.
#' @param alpha Numeric between 0 and 1, indicates the target probability of a type I error.
#' @return list containing three types of objects: (1) data, both raw and summarized observed and expected frequencies,
#' (2) plots, observed and expected frequencies and proportions in bar plot format, and (3), tests the results of the
#' independence and proportion tests.
#' group a
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family xmar functions
#' @export
analyze <- function(data, y, x, title, success = "Traditional", scope,
                    conf = 0.95, alpha = 0.05) {

  # Format data, render bar plots, and conduct hypothesis test
  data <- formatData(data)

  # Conduct test
  test <- zTest(data, success = success, scope = scope, alternative = "two.sided",
                conf = conf, alpha = alpha)

  # Plot Data
  observed <- plotBars(data$observed, y = y, x = x, title = title)
  expected <- plotBars(data$expected, y = y, x = x, title = title)


  results = list(
    data = data,
    plots = list(
      observed = observed,
      expected = expected
    ),
    test = test
  )
  return(results)
}
