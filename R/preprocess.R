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

  #---------------------------------------------------------------------------#
  #                            Period Variable                                #
  #---------------------------------------------------------------------------#
  periodVar <- GSS %>%
    dplyr::mutate(
      Period = ifelse(YEAR < 2000, "Prior to 2000", "Since 2000")) %>%
    dplyr::select(Period)

  # Order factors
  periodVar$Period <- factor(periodVar$Period,
                              levels = c("Prior to 2000", "Since 2000"))


  #---------------------------------------------------------------------------#
  #                           Opinion Variable                                #
  #---------------------------------------------------------------------------#
  opinionVar <- GSS %>%
    dplyr::mutate(
      Opinion = ifelse(XMARSEX %in% c(1,2), "Traditional",
                       ifelse(XMARSEX  %in%  c(3,4),
                              "Non-Traditional", "NA"))) %>%
    dplyr::select(Opinion)

  # Order factors
  opinionVar$Opinion <- factor(opinionVar$Opinion,
                               levels = c("Traditional", "Non-Traditional"))

  #---------------------------------------------------------------------------#
  #                            Gender Variable                                #
  #---------------------------------------------------------------------------#
  genderVar <- GSS %>%
    dplyr::mutate(
      Gender = ifelse(SEX == 1, "Male",
                      ifelse(SEX == 2, "Female", "NA"))) %>%
    dplyr::select(Gender)

  # Order factors
  genderVar$Gender <- factor(genderVar$Gender, levels = c("Male", "Female"))

  #---------------------------------------------------------------------------#
  #                              Age Variable                                 #
  #---------------------------------------------------------------------------#
  ageVar <- GSS %>%
    dplyr::mutate(
      AgeGroup = ifelse(AGE < 25, "15-24",
                        ifelse(AGE < 45, "25-44",
                               ifelse(AGE < 65, "45-64",
                                      ifelse(AGE < 98, "65+", "NA"))))) %>%
    dplyr::select(AgeGroup)

  # Order factors
  ageVar$AgeGroup <- factor(ageVar$AgeGroup,
                            levels = c("15-24", "25-44", "45-64", "65+"))

  #---------------------------------------------------------------------------#
  #                              Class Variable                               #
  #---------------------------------------------------------------------------#
  classVar <- GSS %>%
    dplyr::mutate(
      Class = ifelse(CLASS_ == 1, "Lower Class",
                     ifelse(CLASS_ == 2, "Working Class",
                            ifelse(CLASS_ == 3, "Middle Class",
                                   ifelse(CLASS_ == 4, "Upper Class",
                                          "NA"))))) %>%
    dplyr::select(Class)

  # Order factors
  classVar$Class <- factor(classVar$Class,
                           levels = c("Lower Class", "Working Class",
                                      "Middle Class", "Upper Class"))

  #---------------------------------------------------------------------------#
  #                              Region Variable                              #
  #---------------------------------------------------------------------------#
  regionVar <- GSS %>%
    dplyr::mutate(
      Region = ifelse(REGION %in% c(1,2), "Northeast",
                      ifelse(REGION %in% c(3,4), "Midwest",
                             ifelse(REGION %in% c(5,6,7), "South",
                                    ifelse(REGION == 8, "Mountain",
                                           "Pacific"))))) %>%
    dplyr::select(Region)

  # Order factors
  regionVar$Region <- factor(regionVar$Region,
                            levels = c("Northeast", "South",
                                       "Midwest", "Mountain", "Pacific"))

  #---------------------------------------------------------------------------#
  #                             Gender Data (Bivariate)                       #
  #---------------------------------------------------------------------------#
  # Extract Data (Bivariate)
  gender <- GSS %>%
    dplyr::mutate(
      Period = ifelse(YEAR < 2000, "Prior to 2000", "Since 2000"),
      Gender = ifelse(SEX == 1, "Male",
                      ifelse(SEX == 2, "Female", "NA")),
      Opinion = ifelse(XMARSEX %in% c(1,2), "Traditional",
                       ifelse(XMARSEX  %in%  c(3,4), "Non-Traditional", "NA"))) %>%
    dplyr::select(Period, Gender, Opinion)

  # Filter Data (Bivariate)
  gender <- gender %>% filter(Period != "NA")
  gender <- gender %>% filter(Gender != "NA")
  gender <- gender %>% filter(Opinion != "NA")

  # Order factors
  gender$Period <- factor(gender$Period, levels = c("Prior to 2000", "Since 2000"))
  gender$Gender <- factor(gender$Gender, levels = c("Male", "Female"))
  gender$Opinion <- factor(gender$Opinion, levels = c("Traditional", "Non-Traditional"))

  #---------------------------------------------------------------------------#
  #                             Age Group Data (Bivariate)                    #
  #---------------------------------------------------------------------------#
  # Extract Data (Bivariate)
  ageGroup <- GSS %>%
    dplyr::mutate(
      Period = ifelse(YEAR < 2000, "Prior to 2000", "Since 2000"),
      AgeGroup = ifelse(AGE < 25, "15-24",
                        ifelse(AGE < 45, "25-44",
                               ifelse(AGE < 65, "45-64",
                                      ifelse(AGE < 98, "65+", "NA")))),
      Opinion = ifelse(XMARSEX %in% c(1,2), "Traditional",
                       ifelse(XMARSEX  %in%  c(3,4), "Non-Traditional", "NA"))) %>%
    dplyr::select(Period, AgeGroup, Opinion)

  # Filter Data (Bivariate)
  ageGroup <- ageGroup %>% filter(Period != "NA")
  ageGroup <- ageGroup %>% filter(AgeGroup != "NA")
  ageGroup <- ageGroup %>% filter(Opinion != "NA")

  # Order factors
  ageGroup$Period <- factor(ageGroup$Period,
                            levels = c("Prior to 2000", "Since 2000"))
  ageGroup$Gender <- factor(ageGroup$AgeGroup,
                            levels = c("15-24", "25-44", "45-64", "65+"))
  ageGroup$Opinion <- factor(ageGroup$Opinion,
                             levels = c("Traditional", "Non-Traditional"))

  #---------------------------------------------------------------------------#
  #                   Gender / Age Group Data (Bivariate)                     #
  #---------------------------------------------------------------------------#
  # Extract Data (Bivariate)
  genderAge <- GSS %>%
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
    dplyr::select(Period, GenderAge, Opinion)

  # Filter Data (Bivariate)
  genderAge <- genderAge %>% filter(Period != "NA")
  genderAge <- genderAge %>% filter(GenderAge != "NA")
  genderAge <- genderAge %>% filter(Opinion != "NA")

  # Order factors
  genderAge$Period <- factor(genderAge$Period,
                             levels = c("Prior to 2000", "Since 2000"))
  genderAge$GenderAge <- factor(genderAge$GenderAge,
                             levels = c("Male 15-24", "Male 25-44", "Male 45-64",
                                        "Male 65+",
                                        "Female 15-24", "Female 25-44", "Female 45-64",
                                        "Female 65+"))
  genderAge$Opinion <- factor(genderAge$Opinion,
                              levels = c("Traditional", "Non-Traditional"))

  #---------------------------------------------------------------------------#
  #                         Class Data (Bivariate)                            #
  #---------------------------------------------------------------------------#
  # Extract Data (Bivariate)
  clas <- GSS %>%
    dplyr::mutate(
      Period = ifelse(YEAR < 2000, "Prior to 2000", "Since 2000"),
      Class = ifelse(CLASS_ == 1, "Lower Class",
                     ifelse(CLASS_ == 2, "Working Class",
                            ifelse(CLASS_ == 3, "Middle Class",
                                   ifelse(CLASS_ == 4, "Upper Class",
                                          "NA")))),
      Opinion = ifelse(XMARSEX %in% c(1,2), "Traditional",
                       ifelse(XMARSEX  %in%  c(3,4), "Non-Traditional", "NA"))) %>%
    dplyr::select(Period, Class, Opinion)

  # Filter Data (Bivariate)
  clas <- clas %>% filter(Period != "NA")
  clas <- clas %>% filter(Class != "NA")
  clas <- clas %>% filter(Opinion != "NA")

  # Order factors
  clas$Period <- factor(clas$Period,
                             levels = c("Prior to 2000", "Since 2000"))
  clas$Class <- factor(clas$Class,
                        levels = c("Lower Class", "Working Class",
                                   "Middle Class", "Upper Class"))
  clas$Opinion <- factor(clas$Opinion,
                              levels = c("Traditional", "Non-Traditional"))

  #---------------------------------------------------------------------------#
  #                           Region Data (Bivariate)                         #
  #---------------------------------------------------------------------------#
  # Extract Data (Bivariate)
  region <- GSS %>%
    dplyr::mutate(
      Period = ifelse(YEAR < 2000, "Prior to 2000", "Since 2000"),
      Region = ifelse(REGION %in% c(1,2), "Northeast",
                      ifelse(REGION %in% c(3,4), "Midwest",
                             ifelse(REGION %in% c(5,6,7), "South",
                                    ifelse(REGION == 8, "Mountain", "Pacific")))),
      Opinion = ifelse(XMARSEX %in% c(1,2), "Traditional",
                       ifelse(XMARSEX  %in%  c(3,4), "Non-Traditional", "NA"))) %>%
    dplyr::select(Period, Region, Opinion)

  # Filter Data (Bivariate)
  region <- region %>% filter(Period != "NA")
  region <- region %>% filter(Region != "NA")
  region <- region %>% filter(Opinion != "NA")

  # Order factors
  region$Period <- factor(region$Period,
                         levels = c("Prior to 2000", "Since 2000"))
  region$Region <- factor(region$Region,
                        levels = c("Northeast", "South",
                                   "Midwest", "Mountain", "Pacific"))
  region$Opinion <- factor(region$Opinion,
                          levels = c("Traditional", "Non-Traditional"))

  data <- list(
    univariate = list(
      period = periodVar,
      opinion = opinionVar,
      ageGroup = ageVar,
      gender = genderVar,
      class = classVar,
      region = regionVar
    ),
    bivariate = list(
      ageGroup = ageGroup,
      gender = gender,
      genderAge = genderAge,
      class = clas,
      region = region
    )
  )

  return(data)
}
