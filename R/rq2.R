#==============================================================================#
#                                    rq2                                       #
#==============================================================================#
#' rq2
#'
#' \code{rq2} Performs analysis for research question 2: Are opinions associated
#' with region, political view, class, or education.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family xmar functions
#' @export
#'
rq2 <- function(xmar) {

  rq2Region <- function(xmar) {
    #Set Constants
    target = "Opinions"
    group = "Region"

    # Obtain data
    x <- xmar %>% select(Opinion, Region) %>% filter(Opinion != "NA" & Region != "NA")
    d <- data.frame(Target = factor(x$Opinion),
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

  rq2Class <- function(xmar) {
    #Set Constants
    target = "Opinions"
    group = "Class"

    # Obtain data
    x <- xmar %>% select(Opinion, Class) %>% filter(Opinion != "NA" & Class != "NA")
    d <- data.frame(Target = factor(x$Opinion),
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

  rq2Education <- function(xmar) {
    #Set Constants
    target = "Opinions"
    group = "Education"

    # Obtain data
    x <- xmar %>% select(Opinion, Degree) %>% filter(Opinion != "NA" & Degree != "NA")
    d <- data.frame(Target = factor(x$Opinion),
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

  rq2View <- function(xmar) {
    #Set Constants
    target = "Opinions"
    group = "Political View"

    # Obtain data
    x <- xmar %>% select(Opinion, Views) %>% filter(Opinion != "NA" & Views != "NA")
    d <- data.frame(Target = factor(x$Opinion),
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



  region <- rq2Region(xmar)
  class  <- rq2Class(xmar)
  education  <- rq2Education(xmar)
  view  <- rq2View(xmar)

  results <- list(
    region = region,
    class = class,
    education = education,
    view = view
  )

  return(results)
}
