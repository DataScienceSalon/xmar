#==============================================================================#
#                                 univariate                                   #
#==============================================================================#
#' univariate
#'
#' \code{univariate} Performs univariate analysis of variables
#'
#' @docType function
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family xmar functions
#' @export
#'
univariate <- function(xmar) {

  analyzeDecade = function(xmar) {

    data <- xmar %>% group_by(Decade) %>% dplyr::summarize(N = n()) %>%
      mutate(Proportion = round(N / sum(N), 2), Cumulative = round(cumsum(Proportion), 2))

    plot <- ggplot2::ggplot(data = data,
                                  ggplot2::aes(x = Decade, y = N, fill = Decade)) +
      ggplot2::geom_bar(stat='identity') + ggplot2::theme_minimal(base_size = 24) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.position = "none") +
      ggplot2::scale_fill_brewer(palette = 'Greens') +
      ggplot2::ggtitle('Proportion of Responses by Decade')

    analysis = list(
      data = data,
      plot = plot
    )
    return(analysis)
  }


  analyzeAge = function(xmar) {

    data <- xmar %>% group_by(AgeGroup) %>% dplyr::summarize(N = n()) %>%
      mutate(Proportion = round(N / sum(N), 2), Cumulative = round(cumsum(Proportion), 2))

    plot <- ggplot2::ggplot(data = data,
                                  ggplot2::aes(x = AgeGroup, y = N, fill = AgeGroup)) +
      ggplot2::geom_bar(stat='identity') + ggplot2::theme_minimal(base_size = 24) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.position = "none") +
      ggplot2::scale_fill_brewer(palette = 'Greens') +
      ggplot2::ggtitle('Proportion of Responses by Age Group')

    analysis = list(
      data = data,
      plot = plot
    )
    return(analysis)
  }

  analyzeRegion = function(xmar) {

    data <- xmar %>% group_by(Region) %>% dplyr::summarize(N = n()) %>%
      mutate(Proportion = round(N / sum(N), 2), Cumulative = round(cumsum(Proportion), 2))

    plot <- ggplot2::ggplot(data = data,
                            ggplot2::aes(x = Region, y = N, fill = Region)) +
      ggplot2::geom_bar(stat='identity') + ggplot2::theme_minimal(base_size = 24) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.position = "none") +
      ggplot2::scale_fill_brewer(palette = 'Greens') +
      ggplot2::ggtitle('Proportion of Responses by Region')

    analysis = list(
      data = data,
      plot = plot
    )
    return(analysis)
  }

  analyzeViews = function(xmar) {

    data <- xmar %>% group_by(Views) %>% dplyr::summarize(N = n()) %>%
      mutate(Proportion = round(N / sum(N), 2), Cumulative = round(cumsum(Proportion), 2))

    plot <- ggplot2::ggplot(data = data,
                            ggplot2::aes(x = Views, y = N, fill = Views)) +
      ggplot2::geom_bar(stat='identity') + ggplot2::theme_minimal(base_size = 24) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.position = "none") +
      ggplot2::scale_fill_brewer(palette = 'Greens') +
      ggplot2::ggtitle('Proportion of Responses by Political Views')

    analysis = list(
      data = data,
      plot = plot
    )
    return(analysis)
  }

  analyzeClass = function(xmar) {

    data <- xmar %>% group_by(Class) %>% dplyr::summarize(N = n()) %>%
      mutate(Proportion = round(N / sum(N), 2), Cumulative = round(cumsum(Proportion), 2))

    plot <- ggplot2::ggplot(data = data,
                            ggplot2::aes(x = Class, y = N, fill = Class)) +
      ggplot2::geom_bar(stat='identity') + ggplot2::theme_minimal(base_size = 18) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.position = "none") +
      ggplot2::scale_fill_brewer(palette = 'Greens') +
      ggplot2::ggtitle('Proportion of Responses by Class')

    analysis = list(
      data = data,
      plot = plot
    )
    return(analysis)
  }

  analyzeAttitudes = function(xmar) {

    data <- xmar %>% group_by(Attitude) %>% dplyr::summarize(N = n()) %>%
      mutate(Proportion = round(N / sum(N), 2), Cumulative = round(cumsum(Proportion), 2))

    plot <- ggplot2::ggplot(data = data,
                            ggplot2::aes(x = Attitude, y = N, fill = Attitude)) +
      ggplot2::geom_bar(stat='identity') + ggplot2::theme_minimal(base_size = 18) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.position = "none") +
      ggplot2::scale_fill_brewer(palette = 'Greens') +
      ggplot2::ggtitle('Proportion of Attitudes about Extra-Marital Sex')

    analysis = list(
      data = data,
      plot = plot
    )
    return(analysis)
  }

  analyzeStrays = function(xmar) {

    data <- behaviors %>% group_by(Stray) %>% dplyr::summarize(N = n()) %>%
      mutate(Proportion = round(N / sum(N), 2), Cumulative = round(cumsum(Proportion), 2))

    plot <- ggplot2::ggplot(data = data,
                            ggplot2::aes(x = Stray, y = N, fill = Stray)) +
      ggplot2::geom_bar(stat='identity') + ggplot2::theme_minimal(base_size = 24) +
      ggplot2::theme(text = ggplot2::element_text(family="Open Sans"),
                     axis.title.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     legend.position = "none") +
      ggplot2::scale_fill_brewer(palette = 'Greens') +
      ggplot2::ggtitle('Extra-Marital Sex Proportions')

    analysis = list(
      data = data,
      plot = plot
    )
    return(analysis)
  }


  xmar <- list(
    decade = analyzeDecade(xmar),
    age = analyzeAge(xmar),
    region = analyzeRegion(xmar),
    views = analyzeViews(xmar),
    class = analyzeClass(xmar),
    xmar = analyzeAttitudes(xmar),
    strays = analyzeStrays(xmar)
  )

  return(xmar)
}
