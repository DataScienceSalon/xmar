#==============================================================================#
#                                    rq3                                       #
#==============================================================================#
#' rq3
#'
#' \code{rq3} Performs analysis for research question 2: Are Behaviors associated
#' with region, political view, class, or education.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family xmar functions
#' @export
#'
rq3 <- function(xmar) {

  rq3Region <- function(xmar) {
    #Set Constants
    target = "Behaviors"
    group = "Region"

    # Obtain data
    x <- xmar %>% select(Behavior, Region) %>% filter(Behavior != "NA" & Region != "NA")
    d <- data.frame(Target = factor(x$Behavior),
                    Group = factor(x$Region))

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

  rq3Views <- function(xmar) {
    #Set Constants
    target = "Behaviors"
    group = "Political Views"

    # Obtain data
    x <- xmar %>% select(Behavior, Views) %>% filter(Behavior != "NA" & Views != "NA")
    d <- data.frame(Target = factor(x$Behavior),
                    Group = factor(x$Views))


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

  rq3Class <- function(xmar) {
    #Set Constants
    target = "Behaviors"
    group = "Class"

    # Obtain data
    x <- xmar %>% select(Behavior, Class) %>% filter(Behavior != "NA" & Class != "NA")
    d <- data.frame(Target = factor(x$Behavior),
                    Group = factor(x$Class))


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

  rq3Degree <- function(xmar) {
    #Set Constants
    target = "Behaviors"
    group = "Degree"

    # Obtain data
    x <- xmar %>% select(Behavior, Degree) %>% filter(Behavior != "NA" & Degree != "NA")
    d <- data.frame(Target = factor(x$Behavior),
                    Group = factor(x$Degree))


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

  region <- rq3Region(xmar)
  views <- rq3Views(xmar)
  class <- rq3Class(xmar)
  degree <- rq3Degree(xmar)

  results = list(
    region = region,
    views = views,
    class = class,
    degree = degree
  )
}
