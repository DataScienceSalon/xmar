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
  rqaAge <- function(xmar) {
    #Set Constants
    target = "Opinion"
    group = "Age Group"

    # Obtain data
    x <- xmar %>% select(Opinion, AgeGroup)
    d <- data.frame(Target = factor(x$Opinion),
                    Group = factor(x$AgeGroup))

    # Format data, render bar plots, and conduct hypothesis test
    data <- formatData(d, target = target, group = group)
    plots <- plotBars(data$df, target = target, group = group)
    x2Test <- X2(data$table, target = target, group = group)


    results = list(
      data = data,
      plots = plots,
      x2Test = x2Test
    )
    return(results)
  }
  age <- rqaAge(xmar)

  analysis = list(
    age = age
  )
  return(analysis)
}
