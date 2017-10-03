#==============================================================================#
#                                 univariate                                   #
#==============================================================================#
#' univariate
#'
#' \code{univariate} Performs univariate analysis of variables
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family xmar functions
#' @export
#'
univariate <- function(xmar) {

  me <- 0.05
  alpha <- 0.05
  z <- (1-alpha/2)

  analyzeDegree <- function(xmar) {

    data <- xmar %>% group_by(Degree) %>%
      filter(Degree != "NA") %>%
      summarize(N = n()) %>%
      mutate(`Minimum N` =
               round(qnorm(z) * (N / sum(N) * (1 - (N / sum(N)))) / me^2, 0),
             Proportion = round(N / sum(N), 2),
             Cumulative = round(cumsum(Proportion), 2),
             `Confidence Interval (95%)` =
               paste0("[",
                      round(Proportion -
                              (qnorm(z) *
                                 sqrt(Proportion * (1 - Proportion) / N)),3),
                      ",    ",
                      round(Proportion +
                              (qnorm(z) *
                                 sqrt(Proportion * (1 - Proportion) / N)),3),
                      "]"))
    data$Degree <- factor(data$Degree)


    stats <- xmar %>% summarise(
      Variable = "DEGREE",
      Description = "Highest education degree attained",
      Levels = length(levels(xmar$Degree)),
      Complete = nrow(subset(xmar, xmar$Degree != "NA")),
      NAs = nrow(subset(xmar, xmar$Degree == "NA")),
      Total = n(),
      Rate = paste0(round(Complete / Total * 100, 0), "%")
    )

    # Bar Plot
    barplot <- ggplot2::ggplot(data = data,
                               ggplot2::aes(x = Degree, y = Proportion, fill = Degree)) +
      ggplot2::geom_bar(stat='identity') + ggplot2::theme_minimal(base_size = 24) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.position = "none") +
      ggplot2::scale_fill_brewer(palette = 'Greens') +
      ggplot2::ggtitle('Proportion of Responses by Education Level')

    analysis <- list(
      data = data,
      stats = stats,
      barplot = barplot
    )
    return(analysis)
  }


  analyzeRegion <- function(xmar) {

    data <- xmar %>% group_by(Region) %>%
      filter(Region != "NA") %>%
      summarize(N = n()) %>%
      mutate(`Minimum N` =
               round(qnorm(z) * (N / sum(N) * (1 - (N / sum(N)))) / me^2, 0),
             Proportion = round(N / sum(N), 2),
             Cumulative = round(cumsum(Proportion), 2),
             `Confidence Interval (95%)` =
               paste0("[",
                      round(Proportion -
                              (qnorm(z) *
                                 sqrt(Proportion * (1 - Proportion) / N)),3),
                      ",    ",
                      round(Proportion +
                              (qnorm(z) *
                                 sqrt(Proportion * (1 - Proportion) / N)),3),
                      "]"))
    data$Region <- factor(data$Region)

    stats <- xmar %>% summarise(
      Variable = "REGION",
      Description = "Region of GSS interview",
      Levels = length(levels(xmar$Region)),
      Complete = nrow(subset(xmar, xmar$Region != "NA")),
      NAs = nrow(subset(xmar, xmar$Region == "NA")),
      Total = n(),
      Rate = paste0(round(Complete / Total * 100, 0), "%")
    )

    barplot <- ggplot2::ggplot(data = data,
                               ggplot2::aes(x = Region, y = Proportion, fill = Region)) +
      ggplot2::geom_bar(stat='identity') + ggplot2::theme_minimal(base_size = 24) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.position = "none") +
      ggplot2::scale_fill_brewer(palette = 'Greens') +
      ggplot2::ggtitle('Proportion of Responses by Region')

    analysis <- list(
      data = data,
      stats = stats,
      barplot = barplot
    )
    return(analysis)
  }

  analyzeViews <- function(xmar) {

    data <- xmar %>% group_by(Views) %>%
      filter(Views != "NA") %>%
      summarize(N = n()) %>%
      mutate(`Minimum N` =
               round(qnorm(z) * (N / sum(N) * (1 - (N / sum(N)))) / me^2, 0),
             Proportion = round(N / sum(N), 2),
             Cumulative = round(cumsum(Proportion), 2),
             `Confidence Interval (95%)` =
               paste0("[",
                      round(Proportion -
                              (qnorm(z) *
                                 sqrt(Proportion * (1 - Proportion) / N)),3),
                      ",    ",
                      round(Proportion +
                              (qnorm(z) *
                                 sqrt(Proportion * (1 - Proportion) / N)),3),
                      "]"))
    data$Views <- factor(data$Views)

    stats <- xmar %>% summarise(
      Variable = "POLVIEWS",
      Description = "Political view: liberal, moderate or conservative",
      Levels = length(levels(xmar$Views)),
      Complete = nrow(subset(xmar, xmar$Views != "NA")),
      NAs = nrow(subset(xmar, xmar$Views == "NA")),
      Total = n(),
      Rate = paste0(round(Complete / Total * 100, 0), "%")
    )

    barplot <- ggplot2::ggplot(data = data,
                               ggplot2::aes(x = Views, y = Proportion, fill = Views)) +
      ggplot2::geom_bar(stat='identity') + ggplot2::theme_minimal(base_size = 24) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.position = "none") +
      ggplot2::scale_fill_brewer(palette = 'Greens') +
      ggplot2::ggtitle('Proportion of Responses by Political View')

    analysis <- list(
      data = data,
      stats = stats,
      barplot = barplot
    )
    return(analysis)
  }

  analyzeClass <- function(xmar) {

    data <- xmar %>% group_by(Class) %>%
      filter(Class != "NA") %>%
      summarize(N = n()) %>%
      mutate(`Minimum N` =
               round(qnorm(z) * (N / sum(N) * (1 - (N / sum(N)))) / me^2, 0),
             Proportion = round(N / sum(N), 2),
             Cumulative = round(cumsum(Proportion), 2),
             `Confidence Interval (95%)` =
               paste0("[",
                      round(Proportion -
                              (qnorm(z) *
                                 sqrt(Proportion * (1 - Proportion) / N)),3),
                      ",    ",
                      round(Proportion +
                              (qnorm(z) *
                                 sqrt(Proportion * (1 - Proportion) / N)),3),
                      "]"))
    data$Class <- factor(data$Class)

    stats <- xmar %>% summarise(
      Variable = "CLASS_",
      Description = "Subjective socio-demographic class",
      Levels = length(levels(xmar$Class)),
      Complete = nrow(subset(xmar, xmar$Class != "NA")),
      NAs = nrow(subset(xmar, xmar$Class == "NA")),
      Total = n(),
      Rate = paste0(round(Complete / Total * 100, 0), "%")
    )

    barplot <- ggplot2::ggplot(data = data,
                               ggplot2::aes(x = Class, y = Proportion, fill = Class)) +
      ggplot2::geom_bar(stat='identity') + ggplot2::theme_minimal(base_size = 24) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.position = "none") +
      ggplot2::scale_fill_brewer(palette = 'Greens') +
      ggplot2::ggtitle('Proportion of Responses by Education Level')

    analysis <- list(
      data = data,
      stats = stats,
      barplot = barplot
    )
    return(analysis)
  }

  analyzeOpinions <- function(xmar) {

    data <- xmar %>% group_by(Opinion) %>%
      filter(Opinion != "NA") %>%
      summarize(N = n()) %>%
      mutate(`Minimum N` =
               round(qnorm(z) * (N / sum(N) * (1 - (N / sum(N)))) / me^2, 0),
             Proportion = round(N / sum(N), 2),
             Cumulative = round(cumsum(Proportion), 2),
             `Confidence Interval (95%)` =
               paste0("[",
                      round(Proportion -
                              (qnorm(z) *
                                 sqrt(Proportion * (1 - Proportion) / N)),3),
                      ",    ",
                      round(Proportion +
                              (qnorm(z) *
                                 sqrt(Proportion * (1 - Proportion) / N)),3),
                      "]"))
    data$Opinion <- factor(data$Opinion)

    stats <- xmar %>% summarise(
      Variable = "XMARSEX",
      Description = "OpinionS regarding extra-marital sex",
      Levels = length(levels(xmar$Opinion)),
      Complete = nrow(subset(xmar, xmar$Opinion != "NA")),
      NAs = nrow(subset(xmar, xmar$Opinion == "NA")),
      Total = n(),
      Rate = paste0(round(Complete / Total * 100, 0), "%")
    )

    barplot <- ggplot2::ggplot(data = data,
                               ggplot2::aes(x = Opinion, y = Proportion, fill = Opinion)) +
      ggplot2::geom_bar(stat='identity') + ggplot2::theme_minimal(base_size = 24) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.position = "none") +
      ggplot2::scale_fill_brewer(palette = 'Greens') +
      ggplot2::ggtitle('Proportion of Responses by Opinion')

    analysis <- list(
      data = data,
      stats = stats,
      barplot = barplot
    )
    return(analysis)
  }

  analyzeBehaviors <- function(xmar) {

    data <- xmar %>% group_by(Behavior) %>%
      filter(Behavior != "NA") %>%
      summarize(N = n()) %>%
      mutate(`Minimum N` =
               round(qnorm(z) * (N / sum(N) * (1 - (N / sum(N)))) / me^2, 0),
             Proportion = round(N / sum(N), 2),
             Cumulative = round(cumsum(Proportion), 2),
             `Confidence Interval (95%)` =
               paste0("[",
                      round(Proportion -
                              (qnorm(z) *
                                 sqrt(Proportion * (1 - Proportion) / N)),3),
                      ",    ",
                      round(Proportion +
                              (qnorm(z) *
                                 sqrt(Proportion * (1 - Proportion) / N)),3),
                      "]"))
    data$Behavior <- factor(data$Behavior)

    stats <- xmar %>% summarise(
      Variable = "EVSTRAY",
      Description = "Has the respondent ever had extra-marital sex",
      Levels = length(levels(xmar$Behavior)),
      Complete = nrow(subset(xmar, xmar$Behavior != "NA")),
      NAs = nrow(subset(xmar, xmar$Behavior == "NA")),
      Total = n(),
      Rate = paste0(round(Complete / Total * 100, 0), "%")
    )

    barplot <- ggplot2::ggplot(data = data,
                               ggplot2::aes(x = Behavior, y = Proportion, fill = Behavior)) +
      ggplot2::geom_bar(stat='identity') + ggplot2::theme_minimal(base_size = 24) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.position = "none") +
      ggplot2::scale_fill_brewer(palette = 'Greens') +
      ggplot2::ggtitle('Proportion of Responses by Behavior')

    analysis <- list(
      data = data,
      stats = stats,
      barplot = barplot
    )
    return(analysis)
  }

  # Compile Results
  degree <- analyzeDegree(xmar)
  region <- analyzeRegion(xmar)
  views <- analyzeViews(xmar)
  class <- analyzeClass(xmar)
  opinion <- analyzeOpinions(xmar)
  behavior <- analyzeBehaviors(xmar)
  summary <- rbind(degree$stats, region$stats, views$stats, class$stats,
                   opinion$stats, behavior$stats)

  # Return results
  univariate <- list(
    summary = summary,
    degree = degree,
    region = region,
    views = views,
    class = class,
    opinion = opinion,
    behavior = behavior
  )

  return(univariate)
}
