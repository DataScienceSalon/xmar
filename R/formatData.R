#==============================================================================#
#                                  formatData                                  #
#==============================================================================#
#' formatData
#'
#' \code{formatData} Render frequency and proportion data
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#'
#' @param data Data frame containing proportions comprised of two columns: 'target' and 'group'.
#' 'target' represents the proportions for the target variable and 'group' is the name of the grouping variable
#' @param y Character string indicating the name of the response variable
#' @param x Character string indicating the name of the explanatory variable
#'
#' @return List containing a contingency table and a data frame of descriptive statistics
#'
#' @family xmar functions
#' @export
#'
formatData <- function(data, y, x) {

  #---------------------------------------------------------------------------#
  #                               Prepare Data                                #
  #---------------------------------------------------------------------------#
  # Frequency and Proportion Tables
  tbl <- table(data[[1]], data[[2]], exclude = "NA")
  freqDf <- as.data.frame(ftable(data, exclude = c(NA, "NA")))

  df <- as.data.frame(freqDf %>% arrange(.[[2]], desc(.[[1]])) %>%
    group_by(.[[2]]) %>% mutate(cumFreq = cumsum(Freq),
                                pos = cumFreq - 0.5 * Freq,
                                Ttl = sum(Freq),
                                Prop = Freq / Ttl,
                                Pct = round(Freq / Ttl * 100, 0),
                                cumPct = cumsum(Pct),
                                pos = cumPct - 0.5 * Pct)) %>%
    select(.[[1]],  Freq, cumFreq, pos, Ttl, Prop, Pct, cumPct)

  data = list(
    table = tbl,
    df = df
  )
  return(data)
}
