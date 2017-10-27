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
formatData <- function(data) {

  # Obtain observed and expected frequency data

  dataTable <- table(data[[1]], data[[2]])
  x2 <- chisq.test(dataTable)


  # Frequency Data
  obsDf <- as.data.frame(ftable(data, exclude = c(NA, "NA")))
  expDf <- melt(round(x2$expected, 0))
  names(expDf) <- names(obsDf)
  obsDf$Opinion <- factor(obsDf$Opinion, levels = c("Traditional", "Non-Traditional"))
  expDf$Opinion <- factor(expDf$Opinion, levels = c("Traditional", "Non-Traditional"))


  observed <- as.data.frame(obsDf %>% group_by(.[[2]]) %>%
                              mutate(Ttl = sum(Freq)))
  observed <- as.data.frame(observed %>% arrange(.[[2]], desc(.[[1]])) %>%
                              mutate(pos = Freq - (0.5 * Freq),
                                     Prop = Freq / Ttl,
                                     Pct = round(Freq / Ttl * 100, 0)) %>%
                              select((.[[1]]), Opinion, Freq,
                                     pos, Ttl, Prop, Pct))


  expected <- as.data.frame(expDf %>% group_by(.[[2]]) %>%
                              mutate(Ttl = sum(Freq)))
  expected <- as.data.frame(expected %>% arrange(.[[2]], desc(.[[1]])) %>%
                              mutate(pos = Freq - (0.5 * Freq),
                                     Prop = Freq / Ttl,
                                     Pct = round(Freq / Ttl * 100, 0)) %>%
                              select((.[[1]]), Opinion, Freq,
                                     pos, Ttl, Prop, Pct))

  data = list(
    raw = data,
    table = dataTable,
    observed = observed,
    expected = expected
  )
  return(data)
}
