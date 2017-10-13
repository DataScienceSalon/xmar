#==============================================================================#
#                                    rq1b                                      #
#==============================================================================#
#' rq1b
#'
#' \code{rq1b} Performs analysis for research question 1b: Are the proportions
#' of the population that have engaged in extra-marital conduct the same for
#' those who believe such conduct is "sometimes wrong" and those who
#' consider such conduct to be "not wront at all".
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family xmar functions
#' @export
#'
rq1b <- function(rq1aData) {

  x2Test = function(rq1aData) {

    #---------------------------------------------------------------------------#
    #                             Format Data                                   #
    #---------------------------------------------------------------------------#
    s1 <- rq1aData$data$propDf$Freq[4]
    s2 <- rq1aData$data$propDf$Freq[6]
    n1 <- sum(rq1aData$data$propDf$Freq[3], rq1aData$data$propDf$Freq[4])
    n2 <- sum(rq1aData$data$propDf$Freq[5], rq1aData$data$propDf$Freq[6])

    #---------------------------------------------------------------------------#
    #                                x2Test                                     #
    #---------------------------------------------------------------------------#
    t <- prop.test(x = c(s1, s2), n = c(n1, n2))

    testDf <- data.frame(Test = "Two-Sample Test for Equality of Proportions",
                         Df = t$parameter,
                         X2 = t$statistic,
                         `p-value` = t$p.value,
                         Decision = ifelse(t$p.value > 0.05, "Fail to Reject", "Reject"),
                         row.names = NULL)

    return(testDf)
  }

  zTest = function(rq1aData) {
    #---------------------------------------------------------------------------#
    #                             Format Data                                   #
    #---------------------------------------------------------------------------#
    p1 <- round(rq1aData$data$propDf$Pct[4] / 100, 2)
    p2 <- round(rq1aData$data$propDf$Pct[6] / 100, 2)
    n1 <- sum(rq1aData$data$propDf$Freq[3], rq1aData$data$propDf$Freq[4])
    n2 <- sum(rq1aData$data$propDf$Freq[5], rq1aData$data$propDf$Freq[6])

    #---------------------------------------------------------------------------#
    #                           Compute Z-Test                                  #
    #---------------------------------------------------------------------------#
    pooled <- ((p1 * n1) + (p2 * n2)) / (n1 + n2)
    se <- sqrt(((pooled * (1 - pooled)) / n1) + ((pooled * (1 - pooled)) / n2))
    z <- (p1 - p2) / se
    pValue <- pnorm(z)

    testDf <- data.frame(Test = "Two-Sample z-test for Equality of Proportions",
                         `Point Estimate` = p1 - p2,
                         Null = 0,
                         `Standard Error` = se,
                         `Z-Score` = z,
                         `p-value` = pValue,
                         Decision = ifelse(pValue > 0.05, "Fail to Reject", "Reject"),
                         row.names = NULL)
    return(testDf)

  }

  x2 <- x2Test(rq1aData)
  z <- zTest(rq1aData)

  results <- list(
    x2 = x2,
    z = z
  )
  return(results)
}
