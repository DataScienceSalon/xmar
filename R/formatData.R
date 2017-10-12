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
#' @param target Character string indicating the name of the target variable
#' @param group Character string indicating the name of the grouping variable
#'
#' @family xmar functions
#' @export
#'
formatData <- function(data, target, group) {

  #---------------------------------------------------------------------------#
  #                               Prepare Data                                #
  #---------------------------------------------------------------------------#
  # Frequency and Proportion Tables
  tbl <- table(data$Target, data$Group, dnn = c(target, group))
  freqTbl <- ftable(data, exclude = c(NA, "NA"))
  propTbl <- ftable(prop.table(freqTbl), exclude = c(NA, "NA"))

  # Prepare frequency data frame for plotting
  freqDf <- as.data.frame(freqTbl) %>% arrange(Group, desc(Target)) %>%
    group_by(Group) %>% mutate(cumFreq = cumsum(Freq),
                               pos = cumFreq - 0.5 * Freq)

  # Prepare proportion data frame for plotting
  propDf <- as.data.frame(propTbl) %>% arrange(Group, desc(Target)) %>%
    group_by(Group) %>% mutate(pct = round(Freq * 100, 0),
                               cumPct = cumsum(pct),
                               pos = cumPct - 0.5 * pct)

  # Rename Variables
  freqTbl <- ftable(tbl, exclude = c(NA, "NA"))
  propTbl <- ftable(prop.table(freqTbl), exclude = c(NA, "NA"))
  names(data)[names(data) == "Target"] <- target
  names(data)[names(data) == "Group"] <- group
  names(freqDf)[names(freqDf) == "Target"] <- target
  names(freqDf)[names(freqDf) == "Group"] <- group
  names(propDf)[names(propDf) == "Target"] <- target
  names(propDf)[names(propDf) == "Group"] <- group


  dat <- list(
    data = data,
    table = tbl,
    freqTbl = freqTbl,
    freqDf = freqDf,
    propTbl = propTbl,
    propDf = propDf
  )
  return(dat)
}
