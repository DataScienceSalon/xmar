#==============================================================================#
#                                 multivariate                                 #
#==============================================================================#
#' multivariate
#'
#' \code{multivariate} Performs multivariate analysis of attitudes and
#' behaviors by decade, age group, region, region, political views
#' and class.
#'
#' @docType function
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family xmar functions
#' @export
#'
multivariate <- function(xmar) {

  attitudesByDecade = function(xmar) {

    #-------------------------------------------------------------------------#
    #                             Format Data                                 #
    #-------------------------------------------------------------------------#
    data <- xmar %>% filter(Attitude != "NA") %>% select(Attitude, Decade)
    freqTbl <- ftable(data)
    propTbl <- ftable(prop.table(freqTbl, 2))

    # Prepare frequency data frame for plotting
    dfFreq <- as.data.frame(freqTbl) %>% arrange(Decade, desc(Attitude)) %>%
      group_by(Decade) %>% mutate(cumFreq = cumsum(Freq),
                                      pos = cumFreq - 0.5 * Freq)

    # Prepare proportion data frame for plotting
    dfProp <- as.data.frame(propTbl) %>% arrange(Decade, desc(Attitude)) %>%
      group_by(Decade) %>% mutate(pct = round(Freq * 100, 0),
                                  cumPct = cumsum(pct),
                                  pos = cumPct - 0.5 * pct)

    #-------------------------------------------------------------------------#
    #                             Create Plots                                #
    #-------------------------------------------------------------------------#
    # Frequency Bar Plot
    dfFreqBar <- ggplot2::ggplot() +
      ggplot2::geom_bar(ggplot2::aes(x = Decade, y = Freq, fill = Attitude),
                        data = dfFreq[dfFreq$Attitude != "NA", ], stat = 'identity') +
      ggplot2::geom_text(data = dfFreq[dfFreq$Attitude != "NA", ], ggplot2::aes(x = Decade, y = pos,
                                                       label = Freq),
                         colour="black", family="Tahoma", size=4) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position="bottom",
                     text=ggplot2::element_text(family="Open Sans")) +
      ggplot2::scale_fill_brewer(palette = 'Greens', name = "Attitude") +
      ggplot2::labs(title = "Attitudes About Extra-Marital Sex (Frequencies)",
                    x = "Attitudes by Decade",
                    y = 'Count')

    # Proportion Bar Plot
    dfPropBar <- ggplot2::ggplot() +
      ggplot2::geom_bar(ggplot2::aes(x = Decade, y = pct, fill = Attitude),
                        data = dfProp[dfProp$Attitude != "NA", ], stat = 'identity') +
      ggplot2::geom_text(data = dfProp[dfProp$Attitude != "NA", ],
                         ggplot2::aes(x = Decade, y = pos, label = paste0(pct,"%")),
                         colour="black", family="Tahoma", size=4) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position="bottom",
                     text=ggplot2::element_text(family="Open Sans")) +
      ggplot2::scale_fill_brewer(palette = 'Greens', name = "Attitude") +
      ggplot2::labs(title = "Attitudes About Extra-Marital Sex (Proportions)",
                    x = "Attitudes by Decade",
                    y = 'Proportion')
    #-------------------------------------------------------------------------#
    #                                Tests                                    #
    #-------------------------------------------------------------------------#
    # All Decades
    x2All <- chisq.test(tbl)

    # TODO: Implement chisq comparisons and two-sample proportions of each decade


  }



  xmar <- list(
    decade = analyzeDecade(attitudes),
    age = analyzeAge(attitudes),
    region = analyzeRegion(attitudes),
    views = analyzeViews(attitudes),
    class = analyzeClass(attitudes),
    attitudes = analyzeAttitudes(attitudes),
    strays = analyzeStrays(attitudes)
  )

  return(xmar)
}
