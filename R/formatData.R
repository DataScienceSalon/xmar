#==============================================================================#
#                                  formatData                                  #
#==============================================================================#
#' formatData
#'
#' \code{formatData} Render frequency and proportion data
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#'
#' @param data Data frame containing two or three columns, the values for the
#' response and explanatory variable(s). The first column is the response
#' variable.
#'
#' @return List containing a contingency table and a data frame of descriptive statistics
#'
#' @family xmar functions
#' @export
#'
formatData <- function(data) {

  #---------------------------------------------------------------------------#
  #                               Prepare Data                                #
  #---------------------------------------------------------------------------#

  x2Table <- table(data[[1]], data[[2]])
  x2 <- chisq.test(x2Table)


  # Frequency Data
  obsDf <- as.data.frame(ftable(data, exclude = c(NA, "NA")))
  expDf <- melt(round(x2$expected, 0))
  names(expDf) <- names(obsDf)
  obsDf$Opinion <- factor(obsDf$Opinion, levels = c("Traditional", "Non-Traditional"))
  expDf$Opinion <- factor(expDf$Opinion, levels = c("Traditional", "Non-Traditional"))


  observed <- as.data.frame(obsDf %>% arrange(.[[2]], desc(.[[1]])) %>%
    mutate(cumFreq = cumsum(Freq),
           pos = Freq - (0.5 * Freq),
           Ttl = sum(Freq),
           Prop = Freq / Ttl,
           Pct = round(Freq / Ttl * 100, 0),
           cumPct = cumsum(Pct)) %>%
    select((.[[1]]), Opinion, Freq,
           cumFreq, pos, Ttl, Prop, Pct, cumPct))

  expected <- as.data.frame(expDf %>% arrange(.[[2]], desc(.[[1]])) %>%
    mutate(cumFreq = cumsum(Freq),
           pos = Freq - (0.5 * Freq),
           Ttl = sum(Freq),
           Prop = Freq / Ttl,
           Pct = round(Freq / Ttl * 100, 0),
           cumPct = cumsum(Pct)) %>%
    select((.[[1]]), Opinion, Freq,
           cumFreq, pos, Ttl, Prop, Pct, cumPct))

  data = list(
    raw = data,
    observed = observed,
    expected = expected
  )
  return(data)
}
