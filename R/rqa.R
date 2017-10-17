#==============================================================================#
#                                     rqa                                      #
#==============================================================================#
#' rqa
#'
#' \code{results} Performs the analysis for research question group a, which
#' includes reearch questions 1-3.
#'
#' @param xmar Data frame containing the study data
#' @return list containing the results of hypothesis tests forresearch question
#' group a
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family xmar functions
#' @export
rqa <- function(xmar) {

  # Performs analysis for each reserach question
  analyze <- function(d, y, x, alt = "two.sided") {

    # Format data, render bar plots, and conduct hypothesis test
    data <- formatData(d, y = y, x = x)
    plots <- plotBars(data$df, y = y, x = x)
    x2Test <- X2(data$table, y = y, x = x)
    zTest <- zTest(data$df, y, x, alternative = alt)


    results = list(
      data = data,
      plots = plots,
      x2Test = x2Test,
      zTest = zTest
    )
    return(results)
  }
  age <- analyze(d = (xmar %>% select(Opinion, AgeGroup)),
                y = "Opinion", x = "Age Group",
                alt = "greater")
  gender <- analyze(d = (xmar %>% select(Opinion, Gender)),
                   y = "Opinion", x = "Gender",
                   alt = "two.sided")
  genderAge <- analyze(d = (xmar %>% select(Opinion, GenderAge)),
                       y = "Opinion", x = "Gender and Age Group",
                       alt = "greater")

  analysis = list(
    age = age,
    gender = gender,
    genderAge = genderAge
  )
  return(analysis)
}
