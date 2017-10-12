#==============================================================================#
#                                    rq1                                       #
#==============================================================================#
#' rq1
#'
#' \code{rq1} Performs analysis for research question 1: Are opinions about
#' and behaviors in extra-marital sex associated.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family xmar functions
#' @export
#'
rq1 <- function(xmar) {

  #---------------------------------------------------------------------------#
  #                             Format Data                                   #
  #---------------------------------------------------------------------------#
  d <- xmar %>% select(Opinion, Behavior) %>% filter(Opinion != "NA" &
                                                       Behavior != "NA")

  tbl <- table(d$Opinion, d$Behavior, exclude = "NA", dnn = c("Opinion", "Behavior"))
  freqTbl <- ftable(d, exclude = c(NA, "NA"))

  freqDf <- as.data.frame(freqTbl) %>% arrange(Opinion, desc(Behavior)) %>%
    group_by(Opinion) %>% mutate(cumFreq = cumsum(Freq),
                               pos = cumFreq - 0.5 * Freq)

  propDf <- freqDf %>% group_by(Opinion) %>% mutate(Ttl = sum(Freq))
  propDf <- propDf %>% mutate(Pct = round(Freq / Ttl * 100, 0))
  propDf <- propDf %>% group_by(Opinion) %>%
    mutate(cumPct = cumsum(Pct),
           pos = cumPct - 0.5 * Pct)

  #---------------------------------------------------------------------------#
  #                             Plot Data                                     #
  #---------------------------------------------------------------------------#
  # Frequency Bar Plot
  freqBar <- ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Opinion, y = Freq, fill = Behavior),
                      data = freqDf, stat = 'identity') +
    ggplot2::geom_text(data = freqDf, ggplot2::aes(x = Opinion, y = pos,
                                                        label = Freq),
                       colour="black", family="Tahoma", size = 8) +
    ggplot2::theme_minimal(base_size = 24) +
    ggplot2::theme(legend.position="bottom",
                   text=ggplot2::element_text(family="Open Sans")) +
    ggplot2::scale_fill_brewer(palette = 'Greens', name = "Behavior") +
    ggplot2::labs(title = "Behavior by Opinion (Frequencies)",
                  x = "Opinion",
                  y = "Behavior")

  # Proportion Bar Plot
  propBar <- ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Opinion, y = Pct, fill = Behavior),
                      data = propDf, stat = 'identity') +
    ggplot2::geom_text(data = propDf,
                       ggplot2::aes(x = Opinion, y = pos,
                                    label = paste0(Freq, " (",Pct,"%)")),
                       colour="black", family="Tahoma", size = 8) +
    ggplot2::theme_minimal(base_size = 24) +
    ggplot2::theme(legend.position="bottom",
                   text=ggplot2::element_text(family="Open Sans")) +
    ggplot2::scale_fill_brewer(palette = 'Greens', name = "Behavior") +
    ggplot2::labs(title = "Behavior by Opinion",
                  x = "Opinion",
                  y = "Behavior")

  #---------------------------------------------------------------------------#
  #                         Hypothesis Testing                                #
  #---------------------------------------------------------------------------#

  x2test <- X2(tbl, target = "Behavior", group = "Opinion")

  results = list(
    data = list(
      freqDf = freqDf,
      propDf = propDf
      ),
    plots = list(
      freqBar = freqBar,
      propBar = propBar
      ),
    test = x2test
  )
  return(results)
}
