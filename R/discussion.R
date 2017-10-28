#==============================================================================#
#                                     discussion                               #
#==============================================================================#
#' discussion
#'
#' \code{discussion} Extracts data and renders plots for discussion section.
#'
#' @param analysis Data frame containing the results of analysis and hypothesis tests.
#' @return list containing a data frame of summarized data and a plot.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family xmar functions
#' @export
discussion <- function(analysis) {

  #---------------------------------------------------------------------------#
  #                            Extract Data                                   #
  #---------------------------------------------------------------------------#
  d <- data.frame(Scope = analysis$Scope,
                  p1 = round(analysis$p1, 2),
                  p2 = round(analysis$p2, 2),
                  PctChange = round(analysis$Diff / analysis$p1 * 100, 2),
                  Significant = ifelse(analysis$Decision == "Reject", "Yes", "No"))
  d$Scope <- factor(d$Scope, levels = d$Scope[order(d$PctChange)], ordered = TRUE)
  d <- d %>% arrange(desc(PctChange))
  #---------------------------------------------------------------------------#
  #                              Plot Data                                    #
  #---------------------------------------------------------------------------#
  # Proportion Bar Plot
  plotBar <- ggplot2::ggplot(data = d,
                             ggplot2::aes(x = Scope, y = PctChange,
                                                    fill = Scope)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_text(data = d,
                       ggplot2::aes(x = Scope, y = PctChange / 2,
                                    label = paste0(PctChange, "%")),
                       colour="black", family="Tahoma", size = 8) +
    ggplot2::theme_minimal(base_size = 24) +
    ggplot2::theme(legend.position="bottom",
                   text=ggplot2::element_text(family="Open Sans")) +
    ggplot2::scale_fill_brewer(palette = 'Greens') +
    ggplot2::labs(title = "Percent Change in Traditional Opinion over Periods",
                  x = "Scope",
                  y = "Percent Change")
  res = list(
    data = d,
    plot = plotBar
  )

  return(res)
}
