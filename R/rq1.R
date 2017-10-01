#==============================================================================#
#                                    rfq1                                      #
#==============================================================================#
#' rq1
#'
#' \code{rq1} Performs analysis for research question 1: Are attitudes about
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

  prepareData = function(xmar) {

    data <- xmar %>% select(Attitude, Stray) %>% filter(Attitude != "NA" & Stray != "NA")
    data$Attitude <- factor(data$Attitude)
    data$Stray <- factor(data$Stray)
    tbl <- table(data$Attitude, data$Stray)
    freqTbl <- ftable(data, exclude = c(NA, "NA"))
    propTbl <- ftable(prop.table(freqTbl, 2), exclude = c(NA, "NA"))

    # Prepare frequency data frame for plotting
    freqDf <- as.data.frame(freqTbl) %>% arrange(Attitude, desc(Stray)) %>%
      group_by(Stray) %>% mutate(cumFreq = cumsum(Freq),
                                      pos = cumFreq - 0.5 * Freq)

    # Prepare proportion data frame for plotting
    propDf <- as.data.frame(propTbl) %>% arrange(Stray, desc(Attitude)) %>%
      group_by(Stray) %>% mutate(pct = round(Freq * 100, 0),
                                  cumPct = cumsum(pct),
                                  pos = cumPct - 0.5 * pct)

    data = list(
      table = tbl,
      freqTbl = freqTbl,
      propTbl = propTbl,
      freqDf = freqDf,
      propDf = propDf
    )

    return(data)

  }
  #---------------------------------------------------------------------------#
  #                               Create Plots                                #
  #---------------------------------------------------------------------------#

  preparePlots = function(data) {

    freqDf <- data$freqDf
    propDf <- data$propDf

    # Frequency Bar Plot
    freqDfBar <- ggplot2::ggplot() +
      ggplot2::geom_bar(ggplot2::aes(x = Stray, y = Freq, fill = Attitude),
                        data = freqDf, stat = 'identity') +
      ggplot2::geom_text(data = freqDf, ggplot2::aes(x = Stray, y = pos,
                                                       label = Freq),
                         colour="black", family="Tahoma", size=4) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position="bottom",
                     text=ggplot2::element_text(family="Open Sans")) +
      ggplot2::scale_fill_brewer(palette = 'Greens', name = "Attitude") +
      ggplot2::labs(title = "Extra-Marital Sex Attitudes & Behaviors (Frequencies)",
                    x = "Participated in Extra-Marital Sex",
                    y = 'Attitudes')

    # Proportion Bar Plot
    propDfBar <- ggplot2::ggplot() +
      ggplot2::geom_bar(ggplot2::aes(x = Stray, y = pct, fill = Attitude),
                        data = propDf, stat = 'identity') +
      ggplot2::geom_text(data = propDf,
                         ggplot2::aes(x = Stray, y = pos, label = paste0(pct,"%")),
                         colour="black", family="Tahoma", size=4) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position="bottom",
                     text=ggplot2::element_text(family="Open Sans")) +
      ggplot2::scale_fill_brewer(palette = 'Greens', name = "Attitude") +
      ggplot2::labs(title = "Extra-Marital Sex Attitudes & Behaviors (Proportions)",
                    x = "Participated in Extra-Marital Sex",
                    y = 'Attitudes')

    plots = list(
      freq = freqDfBar,
      prop = propDfBar
    )
    return(plots)
  }
  #---------------------------------------------------------------------------#
  #                                Tests                                      #
  #---------------------------------------------------------------------------#
  runTests = function(data) {

    # Run Test
    x2 <- chisq.test(data$table)

    # Extract Chisq Data
    df   <-x2$parameter
    p    <- 0.05
    reject <- qchisq(p, df, lower.tail = F)

    # Create Table
    table <- data.frame("Test" = "Chi-square test",
                        "DF" = x2$parameter,
                        "Alpha" = 0.05,
                        "95% X2" = reject,
                        "Observed X2" = x2$statistic,
                        "p-Value" = x2$p.value,
                        "Result" = "Reject")

    # Format Plot Data
    gg   <- data.frame(x=seq(0,1.2 * sqrt(x2$statistic),0.001))
    gg$y <- dchisq(gg$x,df)
    ggNotReject <- subset(gg, x <= reject)
    ggReject <- subset(gg, x > reject)
    r1Desc <- data.frame(Result = rep("Fail to Reject", nrow(ggNotReject)))
    r2Desc <- data.frame(Result = rep("Reject", nrow(ggReject)))
    r1 <- cbind(r1Desc, ggNotReject)
    r2 <- cbind(r2Desc, ggReject)
    gg <- rbind(r1,r2)

    # Format Mark of Observed Statistic
    mark <- paste("X = ", round(sqrt(x2$statistic), 2))
    markx <- sqrt(x2$statistic)
    marky <- 0.05
    linex1 <- sqrt(x2$statistic)
    linex2 <- sqrt(x2$statistic)
    liney1 <- marky - 0.02
    liney2 <- 0

    # Render Plot
    plot <- ggplot2::ggplot(gg) +
      ggplot2::theme_minimal() +
      ggplot2::scale_fill_brewer(palette = 'Greens', name = "Result") +
      ggplot2::geom_text(x = markx, y = marky, label = mark, family = "Open Sans",
                         colour="black",size = 4) +
      ggplot2::geom_area(aes(x = x, y = y, fill = Result)) +
      ggplot2::geom_segment(aes(x = linex1, y = liney1, xend = linex2, yend = liney2)) +
      ggplot2::labs(title = "Chi-square Test of Independence Between Extra-Marital Sex Attitudes and Behaviors",
                    y = "Probability")

    test <- list(
      table = table,
      plot = plot
    )


    return(test)
  }

  data <- prepareData(xmar)
  plots <- preparePlots(data)
  test <- runTests(data)

  results = list(
    data = data,
    plots = plots,
    test = test
  )
  return(results)
}
