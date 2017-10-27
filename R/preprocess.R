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
preprocess <- function(GSS) {

  # Create study variables
  xmar <- GSS %>%
    dplyr::mutate(
      Period = ifelse(YEAR < 2000, "Prior to 2000", "Since 2000"),
      AgeGroup = ifelse(AGE < 25, "15-24",
                        ifelse(AGE < 45, "25-44",
                               ifelse(AGE < 65, "45-64",
                                      ifelse(AGE < 98, "65+", "NA")))),
      Gender = ifelse(SEX == 1, "Male",
                      ifelse(SEX == 2, "Female", "NA")),
      GenderAge = paste(Gender, AgeGroup),
      Opinion = ifelse(XMARSEX %in% c(1,2), "Traditional",
                        ifelse(XMARSEX  %in%  c(3,4), "Non-Traditional", "NA"))) %>%
    dplyr::select(Period, AgeGroup, Gender, GenderAge, Opinion)

  # Filter NA values
  xmar <- xmar %>% filter(AgeGroup != "NA")
  xmar <- xmar %>% filter(Gender != "NA")
  xmar <- xmar %>% filter(Opinion != "NA")


  # Create factor levels
  xmar$Period <- factor(xmar$Period, levels = c("Prior to 2000", "Since 2000"))
  xmar$AgeGroup <- factor(xmar$AgeGroup,
                          levels = c("15-24", "25-44", "45-64", "65+"))
  xmar$Gender <- factor(xmar$Gender, levels = c("Male", "Female"))
  xmar$GenderAge <- factor(xmar$GenderAge,
                           levels = c("Male 15-24", "Male 25-44", "Male 45-64",
                                      "Male 65+",
                                      "Female 15-24", "Female 25-44", "Female 45-64",
                                      "Female 65+"))
  xmar$Opinion <- factor(xmar$Opinion, levels = c("Traditional", "Non-Traditional"))

  return(xmar)
}
