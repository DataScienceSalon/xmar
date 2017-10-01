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

  analyzeDegree <- function(xmar) {

    data <- xmar %>% group_by(Degree) %>% dplyr::summarize(N = n()) %>%
      mutate(Proportion = round(N / sum(N), 2),
             Cumulative = round(cumsum(Proportion), 2),
             LowerCI = round(Proportion - (qnorm(.975) *
               sqrt(Proportion * (1 - Proportion) / N)),3),
             UpperCI = round(Proportion + (qnorm(.975) *
               sqrt(Proportion * (1 - Proportion) / N)),3),
             SE = sqrt(Proportion * (1 - Proportion) / N))


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
    barPlot <- ggplot2::ggplot(data = data,
                               ggplot2::aes(x = Degree, y = Proportion, fill = Degree)) +
      ggplot2::geom_bar(stat='identity') + ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.position = "none") +
      ggplot2::scale_fill_brewer(palette = 'Greens') +
      ggplot2::ggtitle('Proportion of Responses by Education Level')

    # Density Plot
    set.seed(23292)
    d <- data.frame()
    data <- subset(data, Degree != "NA")
    data$Degree <- factor(data$Degree)
    for (i in 1:nrow(data)) {
      d <- rbind(d, data.frame(Degree = rep(data$Degree[i], 10000),
                        Proportion = rnorm(n = 10000, mean = data$Proportion[i], sd = data$SE[i])))
    }

    densityPlot <- ggplot2::ggplot(data = d,ggplot2::aes(x = Proportion, fill = Degree)) +
      ggplot2::geom_density() +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.position = "right") +
      ggplot2::scale_fill_brewer(palette = 'Greens') +
      ggplot2::ggtitle('Simulated Sampling Distribution of Proportions by Education Level') +
      ggplot2::labs(x = "Proportion", y = "Density")


    analysis <- list(
      data = data,
      stats = stats,
      barPlot = barPlot,
      densityPlot = densityPlot
    )
    return(analysis)
  }


  analyzeRegion <- function(xmar) {

    data <- xmar %>% group_by(Region) %>% dplyr::summarize(N = n()) %>%
      mutate(Proportion = round(N / sum(N), 2),
             Cumulative = round(cumsum(Proportion), 2),
             LowerCI = round(Proportion - (qnorm(.975) *
                                             sqrt(Proportion * (1 - Proportion) / N)),3),
             UpperCI = round(Proportion + (qnorm(.975) *
                                             sqrt(Proportion * (1 - Proportion) / N)),3),
             SE = sqrt(Proportion * (1 - Proportion) / N))

    stats <- xmar %>% summarise(
      Variable = "REGION",
      Description = "Region of GSS interview",
      Levels = length(levels(xmar$Region)),
      Complete = nrow(subset(xmar, xmar$Region != "NA")),
      NAs = nrow(subset(xmar, xmar$Region == "NA")),
      Total = n(),
      Rate = paste0(round(Complete / Total * 100, 0), "%")
    )

    barPlot <- ggplot2::ggplot(data = data,
                            ggplot2::aes(x = Region, y = Proportion, fill = Region)) +
      ggplot2::geom_bar(stat='identity') + ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.position = "none") +
      ggplot2::scale_fill_brewer(palette = 'Greens') +
      ggplot2::ggtitle('Proportion of Responses by Region')

    # Density Plot
    set.seed(23292)
    d <- data.frame()
    data <- subset(data, Region != "NA")
    data$Region <- factor(data$Region)
    for (i in 1:nrow(data)) {
      d <- rbind(d, data.frame(Region = rep(data$Region[i], 10000),
                               Proportion = rnorm(n = 10000, mean = data$Proportion[i], sd = data$SE[i])))
    }

    densityPlot <- ggplot2::ggplot(data = d,ggplot2::aes(x = Proportion, fill = Region)) +
      ggplot2::geom_density() +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.position = "right") +
      ggplot2::scale_fill_brewer(palette = 'Greens') +
      ggplot2::ggtitle('Simulated Sampling Distribution of Proportions by Region') +
      ggplot2::labs(x = "Proportion", y = "Density")


    analysis <- list(
      data = data,
      stats = stats,
      barPlot = barPlot,
      densityPlot = densityPlot
    )
    return(analysis)
  }

  analyzeViews <- function(xmar) {

    data <- xmar %>% group_by(Views) %>% dplyr::summarize(N = n()) %>%
      mutate(Proportion = round(N / sum(N), 2),
             Cumulative = round(cumsum(Proportion), 2),
             LowerCI = round(Proportion - (qnorm(.975) *
                                             sqrt(Proportion * (1 - Proportion) / N)),3),
             UpperCI = round(Proportion + (qnorm(.975) *
                                             sqrt(Proportion * (1 - Proportion) / N)),3),
             SE = sqrt(Proportion * (1 - Proportion) / N))

    stats <- xmar %>% summarise(
      Variable = "POLVIEWS",
      Description = "Political view: liberal, moderate or conservative",
      Levels = length(levels(xmar$Views)),
      Complete = nrow(subset(xmar, xmar$Views != "NA")),
      NAs = nrow(subset(xmar, xmar$Views == "NA")),
      Total = n(),
      Rate = paste0(round(Complete / Total * 100, 0), "%")
    )

    barPlot <- ggplot2::ggplot(data = data,
                            ggplot2::aes(x = Views, y = Proportion, fill = Views)) +
      ggplot2::geom_bar(stat='identity') + ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.position = "none") +
      ggplot2::scale_fill_brewer(palette = 'Greens') +
      ggplot2::ggtitle('Proportion of Responses by Political View')

    # Density Plot
    set.seed(23292)
    d <- data.frame()
    data <- subset(data, Views != "NA")
    data$Views <- factor(data$Views)
    for (i in 1:nrow(data)) {
      d <- rbind(d, data.frame(Views = rep(data$Views[i], 10000),
                               Proportion = rnorm(n = 10000, mean = data$Proportion[i], sd = data$SE[i])))
    }

    densityPlot <- ggplot2::ggplot(data = d,ggplot2::aes(x = Proportion, fill = Views)) +
      ggplot2::geom_density() +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.position = "right") +
      ggplot2::scale_fill_brewer(palette = 'Greens') +
      ggplot2::ggtitle('Simulated Sampling Distribution of Proportions by Political View') +
      ggplot2::labs(x = "Proportion", y = "Density")


    analysis <- list(
      data = data,
      stats = stats,
      barPlot = barPlot,
      densityPlot = densityPlot
    )
    return(analysis)
  }

  analyzeClass <- function(xmar) {

    data <- xmar %>% group_by(Class) %>% dplyr::summarize(N = n()) %>%
      mutate(Proportion = round(N / sum(N), 2),
             Cumulative = round(cumsum(Proportion), 2),
             LowerCI = round(Proportion - (qnorm(.975) *
                                             sqrt(Proportion * (1 - Proportion) / N)),3),
             UpperCI = round(Proportion + (qnorm(.975) *
                                             sqrt(Proportion * (1 - Proportion) / N)),3),
             SE = sqrt(Proportion * (1 - Proportion) / N))

    stats <- xmar %>% summarise(
      Variable = "CLASS_",
      Description = "Subjective socio-demographic class",
      Levels = length(levels(xmar$Class)),
      Complete = nrow(subset(xmar, xmar$Class != "NA")),
      NAs = nrow(subset(xmar, xmar$Class == "NA")),
      Total = n(),
      Rate = paste0(round(Complete / Total * 100, 0), "%")
    )

    barPlot <- ggplot2::ggplot(data = data,
                            ggplot2::aes(x = Class, y = Proportion, fill = Class)) +
      ggplot2::geom_bar(stat='identity') + ggplot2::theme_minimal(base_size = 18) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.position = "none") +
      ggplot2::scale_fill_brewer(palette = 'Greens') +
      ggplot2::ggtitle('Proportion of Responses by Class')

    # Density Plot
    set.seed(23292)
    d <- data.frame()
    data <- subset(data, Class != "NA")
    data$Class <- factor(data$Class)
    for (i in 1:nrow(data)) {
      d <- rbind(d, data.frame(Class = rep(data$Class[i], 10000),
                               Proportion = rnorm(n = 10000, mean = data$Proportion[i], sd = data$SE[i])))
    }

    densityPlot <- ggplot2::ggplot(data = d,ggplot2::aes(x = Proportion, fill = Class)) +
      ggplot2::geom_density() +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.position = "right") +
      ggplot2::scale_fill_brewer(palette = 'Greens') +
      ggplot2::ggtitle('Simulated Sampling Distribution of Proportions by Class') +
      ggplot2::labs(x = "Proportion", y = "Density")


    analysis <- list(
      data = data,
      stats = stats,
      barPlot = barPlot,
      densityPlot = densityPlot
    )
    return(analysis)
  }

  analyzeOpinions <- function(xmar) {

    data <- xmar %>% group_by(Opinion) %>% dplyr::summarize(N = n()) %>%
      mutate(Proportion = round(N / sum(N), 2),
             Cumulative = round(cumsum(Proportion), 2),
             LowerCI = round(Proportion - (qnorm(.975) *
                                             sqrt(Proportion * (1 - Proportion) / N)),3),
             UpperCI = round(Proportion + (qnorm(.975) *
                                             sqrt(Proportion * (1 - Proportion) / N)),3),
             SE = sqrt(Proportion * (1 - Proportion) / N))

    stats <- xmar %>% summarise(
      Variable = "XMARSEX",
      Description = "Opinion / opinion regarding extra-marital sex",
      Levels = length(levels(xmar$Opinion)),
      Complete = nrow(subset(xmar, xmar$Opinion != "NA")),
      NAs = nrow(subset(xmar, xmar$Opinion == "NA")),
      Total = n(),
      Rate = paste0(round(Complete / Total * 100, 0), "%")
    )

    barPlot <- ggplot2::ggplot(data = data,
                            ggplot2::aes(x = Opinion, y = Proportion, fill = Opinion)) +
      ggplot2::geom_bar(stat='identity') + ggplot2::theme_minimal(base_size = 18) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.position = "none") +
      ggplot2::scale_fill_brewer(palette = 'Greens') +
      ggplot2::ggtitle('Proportion of Opinions about Extra-Marital Sex')

    # Density Plot
    set.seed(23292)
    d <- data.frame()
    data <- subset(data, Opinion != "NA")
    data$Opinion <- factor(data$Opinion)
    for (i in 1:nrow(data)) {
      d <- rbind(d, data.frame(Opinion = rep(data$Opinion[i], 10000),
                               Proportion = rnorm(n = 10000, mean = data$Proportion[i], sd = data$SE[i])))
    }

    densityPlot <- ggplot2::ggplot(data = d,ggplot2::aes(x = Proportion, fill = Opinion)) +
      ggplot2::geom_density() +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.position = "right") +
      ggplot2::scale_fill_brewer(palette = 'Greens') +
      ggplot2::ggtitle('Simulated Sampling Distribution of Proportions by Opinion') +
      ggplot2::labs(x = "Proportion", y = "Density")


    analysis <- list(
      data = data,
      stats = stats,
      barPlot = barPlot,
      densityPlot = densityPlot
    )
    return(analysis)
  }

  analyzeBehaviors <- function(xmar) {

    data <- xmar %>% group_by(Behavior) %>% dplyr::summarize(N = n()) %>%
      mutate(Proportion = round(N / sum(N), 2),
             Cumulative = round(cumsum(Proportion), 2),
             LowerCI = round(Proportion - (qnorm(.975) *
                                             sqrt(Proportion * (1 - Proportion) / N)),3),
             UpperCI = round(Proportion + (qnorm(.975) *
                                             sqrt(Proportion * (1 - Proportion) / N)),3),
             SE = sqrt(Proportion * (1 - Proportion) / N))

    stats <- xmar %>% summarise(
      Variable = "EVSTRAY",
      Description = "Has the respondent ever had extra-marital sex",
      Levels = length(levels(xmar$Behavior)),
      Complete = nrow(subset(xmar, xmar$Behavior != "NA")),
      NAs = nrow(subset(xmar, xmar$Behavior == "NA")),
      Total = n(),
      Rate = paste0(round(Complete / Total * 100, 0), "%")
    )

    barPlot <- ggplot2::ggplot(data = data,
                            ggplot2::aes(x = Behavior, y = Proportion, fill = Behavior)) +
      ggplot2::geom_bar(stat='identity') + ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.position = "none") +
      ggplot2::scale_fill_brewer(palette = 'Greens') +
      ggplot2::ggtitle('Extra-Marital Sex Proportions')

    # Density Plot
    set.seed(23292)
    d <- data.frame()
    data <- subset(data, Behavior != "NA")
    data$Behavior <- factor(data$Behavior)
    for (i in 1:nrow(data)) {
      d <- rbind(d, data.frame(Behavior = rep(data$Behavior[i], 10000),
                               Proportion = rnorm(n = 10000, mean = data$Proportion[i], sd = data$SE[i])))
    }

    densityPlot <- ggplot2::ggplot(data = d,ggplot2::aes(x = Proportion, fill = Behavior)) +
      ggplot2::geom_density() +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.position = "right") +
      ggplot2::scale_fill_brewer(palette = 'Greens') +
      ggplot2::ggtitle('Simulated Sampling Distribution of Proportions Who Behavior') +
      ggplot2::labs(x = "Proportion", y = "Density")


    analysis <- list(
      data = data,
      stats = stats,
      barPlot = barPlot,
      densityPlot = densityPlot
    )
    return(analysis)
  }

  # Compile Results
  degree <- analyzeDegree(xmar)
  region <- analyzeRegion(xmar)
  views <- analyzeViews(xmar)
  class <- analyzeClass(xmar)
  opinions <- analyzeOpinions(xmar)
  strays <- analyzeBehaviors(xmar)
  summary <- rbind(degree$stats, region$stats, views$stats, class$stats,
                   opinions$stats, strays$stats)

  # Return results
  univariate <- list(
    summary = summary,
    degree = degree,
    region = region,
    views = views,
    class = class,
    opinions = opinions,
    strays = strays
  )

  return(univariate)
}
