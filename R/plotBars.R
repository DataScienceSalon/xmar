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
  #                               Prepare Data                                #
  #---------------------------------------------------------------------------#
  # Frequency and Proportion Tables
  freqTbl <- ftable(data, exclude = c(NA, "NA"))
  propTbl <- ftable(prop.table(freqTbl, 2), exclude = c(NA, "NA"))

  # Prepare frequency data frame for plotting
  freqDf <- as.data.frame(freqTbl) %>% arrange(Group, desc(Target)) %>%
    group_by(Group) %>% mutate(cumFreq = cumsum(Freq),
                               pos = cumFreq - 0.5 * Freq)

  # Prepare proportion data frame for plotting
  propDf <- as.data.frame(propTbl) %>% arrange(Group, desc(Target)) %>%
    group_by(Group) %>% mutate(pct = round(Freq * 100, 0),
                               cumPct = cumsum(pct),
                               pos = cumPct - 0.5 * pct)


  #---------------------------------------------------------------------------#
  #                               Create Plots                                #
  #---------------------------------------------------------------------------#
  # Frequency Bar Plot
  freqBar <- ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Group, y = Freq, fill = Target),
                      data = freqDf, stat = 'identity') +
    ggplot2::geom_text(data = freqDf, ggplot2::aes(x = Group, y = pos,
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
    ggplot2::geom_bar(ggplot2::aes(x = Group, y = pct, fill = Target),
                      data = propDf, stat = 'identity') +
    ggplot2::geom_text(data = propDf,
                       ggplot2::aes(x = Group, y = pos, label = paste0(pct,"%")),
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
