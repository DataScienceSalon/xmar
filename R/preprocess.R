#==============================================================================#
#                                 preprocess                                   #
#==============================================================================#
#' preprocess
#'
#' \code{preprocess} Creates additional variables required for analysis.
#'
#' @docType function
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family xmar functions
#' @export
#'
preprocess <- function(GSS) {

  xmar <- GSS %>%
    dplyr::mutate(
      AgeGroup = ifelse(AGE < 25, "15-24",
                        ifelse(AGE < 35, "25-34",
                               ifelse(AGE < 45, "35-44",
                                      ifelse(AGE < 55, "45-54",
                                             ifelse(AGE < 65, "55-64", "65+"))))),
      Decade = ifelse(YEAR < 1980, "1970's",
                      ifelse(YEAR < 1990, "1980's",
                             ifelse(YEAR < 2000, "1990's",
                                    ifelse(YEAR < 2010, "2000's", "2010's")))),
      Region = ifelse(REGION == 1, "New England",
                      ifelse(REGION == 2, "Mid Atlantic",
                             ifelse(REGION < 5, "North Mid-West",
                                    ifelse(REGION == 5, "South Atlantic",
                                           ifelse(REGION < 8, "South Mid-West",
                                                  ifelse(REGION == 8, "Mountain", "Pacific")))))),
      Views = ifelse(POLVIEWS < 4, "Liberal",
                     ifelse(POLVIEWS == 4, "Moderate",
                            ifelse(POLVIEWS < 8, "Conservative", "NA"))),
      Class = ifelse(CLASS_ == 1, "Lower Class",
                     ifelse(CLASS_ == 2, "Working Class",
                            ifelse(CLASS_ == 3, "Middle Class",
                                   ifelse(CLASS_ == 4, "Upper Class", "NA")))),
      Attitude = ifelse(XMARSEX == 1, "Always Wrong",
                        ifelse(XMARSEX == 2, "Almost Always Wrong",
                               ifelse(XMARSEX == 3, "Sometimes Wrong",
                                      ifelse(XMARSEX == 4, "Not Wrong At All", "NA")))),
      Stray = ifelse(EVSTRAY == 1, "Yes",
                     ifelse(EVSTRAY == 2, "No", "NA")))

  xmar$AgeGroup <- factor(xmar$AgeGroup, levels = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"))
  xmar$Decade <- factor(xmar$Decade, levels = c("1970's", "1980's", "1990's", "2000's", "2010's"))
  xmar$Region <- factor(xmar$Region, levels = c("New England", "Mid Atlantic", "South Atlantic",
                                                "North Mid-West", "South Mid-West", "Mountain", "Pacific"))
  xmar$Views <- factor(xmar$Views, levels = c("Liberal", "Moderate", "Conservative", "NA"))
  xmar$Class <- factor(xmar$Class, levels = c("Lower Class", "Working Class", "Middle Class", "Upper Class", "NA"))
  xmar$Attitude <- factor(xmar$Attitude, levels = c("Always Wrong", "Almost Always Wrong", "Sometimes Wrong", "Not Wrong At All", "NA"))
  xmar$Stray <- factor(xmar$Stray, levels = c("Yes", "No", "NA"))
  return(xmar)
}
