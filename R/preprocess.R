#==============================================================================#
#                                 preprocess                                   #
#==============================================================================#
#' preprocess
#'
#' \code{preprocess} Creates additional variables required for analysis.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family xmar functions
#' @export
#'
preprocess <- function(GSS) {

  # Filter by date and convert values from numeric to descriptive
  xmar <- GSS %>% filter(YEAR %in% c(2010:2017)) %>%
    dplyr::mutate(
      AgeGroup = ifelse(AGE < 25, "15-24",
                        ifelse(AGE < 35, "25-34",
                               ifelse(AGE < 45, "35-44",
                                      ifelse(AGE < 55, "45-54",
                                             ifelse(AGE < 65, "55-64",
                                                    ifelse(AGE < 98, "65+", "NA")))))),
      Region = ifelse(REGION == 1, "New England",
                      ifelse(REGION == 2, "Mid Atlantic",
                             ifelse(REGION %in% c(3,4), "North Mid-West",
                                    ifelse(REGION == 5, "South Atlantic",
                                           ifelse(REGION %in% c(6,7), "South Mid-West",
                                                  ifelse(REGION == 8, "Mountain", "Pacific")))))),
      Degree = ifelse(DEGREE == 0, "Less Than High School",
                      ifelse(DEGREE == 1, "High School",
                             ifelse(DEGREE == 2, "Junior College",
                                    ifelse(DEGREE == 3, "Bachelor",
                                           ifelse(DEGREE == 4, "Graduate", "NA"))))),
      Views = ifelse(POLVIEWS %in% c(1,2,3), "Liberal",
                     ifelse(POLVIEWS == 4, "Moderate",
                            ifelse(POLVIEWS %in% c(5,6,7), "Conservative", "NA"))),
      Class = ifelse(CLASS_ == 1, "Lower Class",
                     ifelse(CLASS_ == 2, "Working Class",
                            ifelse(CLASS_ == 3, "Middle Class",
                                   ifelse(CLASS_ == 4, "Upper Class", "NA")))),
      Attitude = ifelse(XMARSEX %in% c(1,2), "Wrong",
                        ifelse(XMARSEX == 3, "Sometimes Wrong",
                               ifelse(XMARSEX == 4, "Not Wrong At All", "NA"))),
      Stray = ifelse(EVSTRAY == 1, "Yes",
                     ifelse(EVSTRAY == 2,"No", "NA")))

  # Create factor levels
  xmar$AgeGroup <- factor(xmar$AgeGroup,
                          levels = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"))
  xmar$Region <- factor(xmar$Region,
                        levels = c("New England", "Mid Atlantic", "South Atlantic",
                                                "North Mid-West", "South Mid-West", "Mountain", "Pacific"))
  xmar$Degree <- factor(xmar$Degree,
                        levels = c("Less Than High School", "High School", "Junior College", "Bachelor", "Graduate", "NA"))
  xmar$Views <- factor(xmar$Views,
                       levels = c("Liberal", "Moderate", "Conservative", "NA"))
  xmar$Class <- factor(xmar$Class,
                       levels = c("Lower Class", "Working Class", "Middle Class", "Upper Class", "NA"))
  xmar$Attitude <- factor(xmar$Attitude,
                          levels = c("Wrong", "Sometimes Wrong", "Not Wrong At All", "NA"))
  xmar$Stray <- factor(xmar$Stray,
                       levels = c("Yes", "No", "NA"))
  return(xmar)
}
