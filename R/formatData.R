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
  tbl <- table(data$Target, data$Group, exclude = "NA")
  freqTbl <- ftable(data, exclude = c(NA, "NA"))

  df <- as.data.frame(freqTbl) %>% arrange(Group, desc(Target)) %>%
    group_by(Group) %>% mutate(cumFreq = cumsum(Freq),
                                 pos = cumFreq - 0.5 * Freq)

  df <- df %>% group_by(Group) %>% mutate(Ttl = sum(Freq))
  df <- df %>% mutate(Pct = round(Freq / Ttl * 100, 0))
  df <- df %>% group_by(Group) %>%
    mutate(cumPct = cumsum(Pct),
           pos = cumPct - 0.5 * Pct)

  data = list(
    table = tbl,
    df = df
  )
  return(data)
}
