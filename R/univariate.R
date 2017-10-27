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
univariate <- function(xmar) {

  analyze <- function(data, var) {

    me <- 0.05
    alpha <- 0.05
    z <- (1-alpha/2)

    stats <- data %>%
      mutate(Level = data[,1],
             `Minimum N` = round(p * (1-p) / (me / qnorm(z))^2, 0),
             N = N,
             Proportion = round(p, 2),
             Cumulative = round(cumsum(Proportion), 2),
             `Confidence Interval (95%)` =
               paste0("[",
                      round(Proportion -
                              (qnorm(z) *
                                 sqrt(Proportion * (1 - Proportion) / N)),3),
                      ", ",
                      round(Proportion +
                              (qnorm(z) *
                                 sqrt(Proportion * (1 - Proportion) / N)),3),
                      "]"),
             pos = N / 2)

    barplot <- ggplot2::ggplot(data = stats,
                               ggplot2::aes(x = Level, y = N, fill = Level)) +
      ggplot2::geom_bar(stat='identity') + ggplot2::theme_minimal(base_size = 24) +
      ggplot2::geom_text(
        data = stats,
        ggplot2::aes(x = Level, y = pos,
                     label = paste0(N, " (",round(Proportion * 100, 0),"%)")),
        colour="black", family="Tahoma", size = 8) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.position = "none") +
      ggplot2::scale_fill_brewer(palette = 'Greens') +
      ggplot2::ggtitle(paste('Frequency and Proportion of Responses by', var))

    stats <- stats %>% select(Level, `Minimum N`, N, Proportion, Cumulative,
                              `Confidence Interval (95%)`)

    analysis <- list(
      stats = stats,
      plot = barplot
    )

    return(analysis)
  }

  # Format Data
  opinion <- as.data.frame(xmar %>% select(Opinion) %>% group_by(Opinion) %>%
                           summarize(N = n()) %>%
                           mutate(p = N/ sum(N)), row.names = NULL)

  period <- as.data.frame(xmar %>% select(Period) %>% group_by(Period) %>%
                           summarize(N = n()) %>%
                           mutate(p = N/ sum(N)), row.names = NULL)
  age <- as.data.frame(xmar %>% select(AgeGroup) %>% group_by(AgeGroup) %>%
                         summarize(N = n()) %>%
                         mutate(p = N/ sum(N)), row.names = NULL)
  gender <- as.data.frame(xmar %>% select(Gender) %>% group_by(Gender) %>%
                            summarize(N = n()) %>%
                            mutate(p = N/ sum(N)), row.names = NULL)
  maleAge <- as.data.frame(xmar %>% filter(Gender == "Male") %>%
                             group_by(GenderAge) %>%
                             summarize(N = n()) %>%
                             mutate(p = N/ sum(N)), row.names = NULL)
  femaleAge <- as.data.frame(xmar %>% filter(Gender == "Female") %>%
                             group_by(GenderAge) %>%
                             summarize(N = n()) %>%
                             mutate(p = N/ sum(N)), row.names = NULL)

  # Conduct Analysis
  opinion <- analyze(opinion, "Opinion")
  period <- analyze(period, "Period")
  age <- analyze(age, "Age Group")
  gender <- analyze(gender, "Gender")
  maleAge <- analyze(maleAge, "Males and Age Group")
  femaleAge <- analyze(femaleAge, "Females and Age Group")

  # Return analysis
  analysis <- list(
    opinion = opinion,
    period = period,
    age = age,
    gender = gender,
    maleAge = maleAge,
    femaleAge = femaleAge
    )
  return(analysis)
}
