#==============================================================================#
#                                    rq1                                       #
#==============================================================================#
#' rq1
#'
#' \code{rq1} Performs analysis for research question 1: Are opinions about
#' and behaviors in extra-marital sex associated.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family xmar functions
#' @export
#'
rq1 <- function(xmar) {

  #Set Constants
  target = "Opinions"
  group = "Behavior"

  # Obtain data
  x <- xmar %>% select(Opinion, Behavior) %>% filter(Opinion != "NA" & Behavior != "NA")
  d <- data.frame(Target = factor(x$Opinion),
                  Group = factor(x$Behavior))

  # Format data, render bar plots, and conduct hypothesis test
  data <- formatData(d, target = target, group = group)
  plots <- plotBars(data$data, target = target, group = group)
  test <- X2(data$table, target = target, group = group)

  results = list(
    data = data,
    plots = plots,
    test = test
  )
  return(results)
}
