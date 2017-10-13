#==============================================================================#
#                                    rq3                                       #
#==============================================================================#
#' rq3
#'
#' \code{rq3} Performs analysis for research question 2: Are opinions associated
#' with region, political view, class, or education.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family xmar functions
#' @export
#'
rq3 <- function(xmar) {

  rq3Region <- function(xmar) {
    #Set Constants
    target = "Behavior"
    group = "Region"

    # Obtain data
    x <- xmar %>% select(Behavior, Region) %>% filter(Behavior != "NA" & Region != "NA")
    d <- data.frame(Target = factor(x$Behavior),
                    Group = factor(x$Region))

    # Format data, render bar plots, and conduct hypothesis test
    data <- formatData(d, target = target, group = group)
    plots <- plotBars(data$df, target = target, group = group)
    test <- X2(data$table, target = target, group = group)

    results = list(
      data = data,
      plots = plots,
      test = test
    )
    return(results)
  }

  rq3Class <- function(xmar) {
    #Set Constants
    target = "Behavior"
    group = "Class"

    # Obtain data
    x <- xmar %>% select(Behavior, Class) %>% filter(Behavior != "NA" & Class != "NA")
    d <- data.frame(Target = factor(x$Behavior),
                    Group = factor(x$Class))

    # Format data, render bar plots, and conduct hypothesis test
    data <- formatData(d, target = target, group = group)
    plots <- plotBars(data$df, target = target, group = group)
    test <- X2(data$table, target = target, group = group)

    results = list(
      data = data,
      plots = plots,
      test = test
    )
    return(results)
  }

  rq3Education <- function(xmar) {
    #Set Constants
    target = "Behavior"
    group = "Education"

    # Obtain data
    x <- xmar %>% select(Behavior, Degree) %>% filter(Behavior != "NA" & Degree != "NA")
    d <- data.frame(Target = factor(x$Behavior),
                    Group = factor(x$Degree))

    # Format data, render bar plots, and conduct hypothesis test
    data <- formatData(d, target = target, group = group)
    plots <- plotBars(data$df, target = target, group = group)
    test <- X2(data$table, target = target, group = group)

    results = list(
      data = data,
      plots = plots,
      test = test
    )
    return(results)
  }

  rq3View <- function(xmar) {
    #Set Constants
    target = "Behavior"
    group = "Political View"

    # Obtain data
    x <- xmar %>% select(Behavior, Views) %>% filter(Behavior != "NA" & Views != "NA")
    d <- data.frame(Target = factor(x$Behavior),
                    Group = factor(x$Views))

    # Format data, render bar plots, and conduct hypothesis test
    data <- formatData(d, target = target, group = group)
    plots <- plotBars(data$df, target = target, group = group)
    test <- X2(data$table, target = target, group = group)

    results = list(
      data = data,
      plots = plots,
      test = test
    )
    return(results)
  }



  region <- rq3Region(xmar)
  class  <- rq3Class(xmar)
  education  <- rq3Education(xmar)
  view  <- rq3View(xmar)

  results <- list(
    region = region,
    class = class,
    education = education,
    view = view
  )

  return(results)
}
