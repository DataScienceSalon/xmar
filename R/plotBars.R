#==============================================================================#
#                                    plotBars                                  #
#==============================================================================#
#' plotBars
#'
#' \code{plotBars} Renders segmented bar plots with value labels representing
#' proportions each segment represents.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#'
#' @param data Data frame containing proportions comprised of two columns: 'target' and 'group'.
#' 'target' represents the proportions for the target variable and 'group' is the name of the grouping variable
#' @param target Character string indicating the name of the target variable.
#' @param group Character string indicating the name of the grouping variable.
#'
#' @family xmar functions
#' @export
#'
plotBars <- function(data, target, group) {

  #---------------------------------------------------------------------------#
  #                               Create Plots                                #
  #---------------------------------------------------------------------------#
  # Frequency Bar Plot
  freqBar <- ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Behavior, y = Freq, fill = Opinions),
                      data = data, stat = 'identity') +
    ggplot2::geom_text(data = data, ggplot2::aes(x = Behavior, y = pos,
                                                   label = Freq),
                       colour="black", family="Tahoma", size=4) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position="bottom",
                   text=ggplot2::element_text(family="Open Sans")) +
    ggplot2::scale_fill_brewer(palette = 'Greens', name = target) +
    ggplot2::labs(title = paste(target, "by", group, "(Frequencies)"),
                  x = group,
                  y = target)

  # Proportion Bar Plot
  propBar <- ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = group, y = pct, fill = target),
                      data = data$propDf, stat = 'identity') +
    ggplot2::geom_text(data = data$propDf,
                       ggplot2::aes(x = group, y = pos, label = paste0(pct,"%")),
                       colour="black", family="Tahoma", size=4) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position="bottom",
                   text=ggplot2::element_text(family="Open Sans")) +
    ggplot2::scale_fill_brewer(palette = 'Greens', name = target) +
    ggplot2::labs(title = paste(target, "by", group, "(Proportions)"),
                  x = group,
                  y = target)

  plotBars <- list(
    freqBar = freqBar,
    propBar = propBar
  )
  return(plotBars)
}
