#==============================================================================#
#                                   zTest                                      #
#==============================================================================#
#' zTest
#'
#' \code{zTest} Conducts a z-tests difference in proportions for a dichotomous
#' categorical explanatory variable on a response variable. Assumes the null
#' hypothesis is equal proportions.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#'
#' @param data Explanatory and response variable values
#' @param success Which level of response variable to call "success", i.e. upon which inference should be done. Defaults to order in which factor variables are defined.
#' @param scope Character string indicating the scope of the data being analyzed.
#' @param alternative direction of the alternative hypothesis; "less","greater", or "two.sided"
#' @param conf Desired confidence level
#' @param alpha Probability of a type I error
#'
#' @return Data frame containing the results of the z-test
#'
#' @family xmar functions
#' @export
zTest <- function(data, success = "Traditional", scope,
                  alternative = "two.sided", conf = 0.95, alpha = 0.05) {

  #---------------------------------------------------------------------------#
  #                                 PropTest                                  #
  #---------------------------------------------------------------------------#
  groups <- as.character((data$observed %>% filter(.[[2]] == success) %>% select(Period))$Period)
  pSuccess <- (data$observed %>% filter(.[[2]] == success) %>% select(Prop))$Prop
  nSuccess <- (data$observed %>% filter(.[[2]] == success) %>% select(Freq))$Freq
  nTotal <- (data$observed %>% group_by(.[[1]]) %>% summarize(Ttl = sum(Freq)) %>% select(Ttl))$Ttl
  t <- prop.test(x = nSuccess, n = nTotal, correct = FALSE, alternative = "two.sided")

  #---------------------------------------------------------------------------#
  #                                  zTest                                    #
  #---------------------------------------------------------------------------#
  pPooled <- sum(nSuccess) / sum(nTotal)
  sePooled <- sqrt((pPooled * (1-pPooled) / nTotal[1]) + (pPooled * (1-pPooled) / nTotal[2]))
  zScore <- abs((pSuccess[1] - pSuccess[2]) / sePooled)
  pValue <- 2 * pnorm(-abs(zScore))

  #---------------------------------------------------------------------------#
  #                             Results Table                                 #
  #---------------------------------------------------------------------------#
  z <- list(
    Test = ifelse(alternative == "less", "One-Sided, Two-Sample Proportion Test",
                   ifelse(alternative == "greater", "One-Sided, Two-Sample Proportion Test",
                          "Two-Sample Proportion Test")),
    Groups = paste0(groups[1], "/", groups[2]),
    Scope = scope,
    H0 = paste0("H0: p1 = p2"),
    Ha = ifelse(alternative == "less", "p1 < p2",
                ifelse(alternative == "greater",  "p1 > p2", "p1 <> p2")),
    Alpha = alpha,
    p1 = as.numeric(round(pSuccess[1], 2)),
    p2 = as.numeric(round(pSuccess[2], 2)),
    Diff = as.numeric(round(abs(pSuccess[1] - pSuccess[2]), 2)),
    zScore = round(zScore,2),
    pValue = as.numeric(round(t$p.value, 4)),
    CI = paste0("[ ", round(t$conf.int[1], 3), " , ", round(t$conf.int[2], 3), " ]"),
    Decision = ifelse(t$p.value < alpha, "Reject", "Fail to Reject")
  )


  #---------------------------------------------------------------------------#
  #                         Statement of Results                              #
  #---------------------------------------------------------------------------#
  # Determine group with highest proportion
  topGroup <- ifelse(pSuccess[1] > pSuccess[2], groups[1], groups[2])
  group1 <- data$observed %>% filter(.[[1]] == topGroup)
  group2 <- data$observed %>% filter(.[[1]] != topGroup)

  # Extract values for text
  g1  <- (unique(group1 %>% select(Period))$Period)
  p1  <- (group1 %>% filter(Opinion == "Traditional") %>% select(Prop))$Prop
  p1a <- (group1 %>% filter(Opinion == "Non-Traditional") %>% select(Prop))$Prop
  f1  <- (group1 %>% filter(Opinion == "Traditional") %>% select(Freq))$Freq
  f1a <-(group1 %>% filter(Opinion == "Non-Traditional") %>% select(Freq))$Freq
  t1 <- f1 + f1a

  g2  <- (unique(group2 %>% select(Period))$Period)
  p2  <- (group2 %>% filter(Opinion == "Traditional") %>% select(Prop))$Prop
  p2a <- (group2 %>% filter(Opinion == "Non-Traditional") %>% select(Prop))$Prop
  f2  <- (group2 %>% filter(Opinion == "Traditional") %>% select(Freq))$Freq
  f2a <-(group2 %>% filter(Opinion == "Non-Traditional") %>% select(Freq))$Freq
  t2 <- f2 + f2a

  # Compute risk ratio
  if (pSuccess[1] > pSuccess[2]) {
    rr <- epitools::riskratio(x = t(data$table), method = "wald", conf.level = conf)
  } else {
    rr <- epitools::riskratio(x = t(data$table), method = "wald", conf.level = conf, rev = "columns")
  }

  alt <- ifelse(alternative == "two.sided", "not equal to",
                ifelse(alternative == "less", "less than","greater than"))
  ciNote <- ifelse(z$Decision == "Reject",
                   paste0("Further, the confidence interval for the difference in ",
                          "proportions does not include zero, the null ",
                          "hypothesis value. As such, the null hypothesis ",
                          "is rejected and both the hypothesis test and ",
                          "confidence interval methods agree. "),
                   paste0("Further, the confidence interval for the difference in ",
                          "proportions includes zero, the null ",
                          "hypothesis value. As such, the null hypothesis ",
                          "is not rejected and both the hypothesis test and ",
                          "confidence interval methods agree. "))

  # Format Statements
  stmt <- list()
  stmt$type <- paste0("This was a ", (conf * 100), "% confidence, two-proportion z-test ",
                      "of the null hypothesis that the true population proportion of ",
                      "traditional opinion within the ", tolower(scope), " population is equal for ",
                      "the periods prior to, and since, the year 2000. ")

  if ((alternative == "two.sided" & pValue < (alpha / 2))
       | (alternative != "two.sided" & pValue < alpha)) {
    stmt$conclude <- paste0("Therefore, the null hypothesis was rejected with, ",
                            conf * 100, "% confidence, in favor ",
                            "of the alternative hypothesis that the true ",
                            "population proportion of traditional opinion ",
                            "within the ", tolower(scope), " population ",
                            tolower(g1), " is ", alt, " the true population ",
                            "proportion of traditional opinion within the ",
                            tolower(scope), " population ", tolower(g2), ". ",
                            "Moreover, one can conclude with ", conf * 100,
                            "% confidence, that a randomly selected person from the ",
                            tolower(scope), " population ", tolower(g1), " is ",
                            round(rr$measure[[2]], 2), " times as likely to ",
                            "hold traditional opinions than such a randomly ",
                            "selected person from the ", tolower(scope),
                            " population ", tolower(g2), ". ")
  } else {
    stmt$conclude <- paste0("Therefore, the null hypothesis was not rejected; ",
                            "consequently, one concludes, with ", conf * 100,
                            "% confidence, that the true proportion of ",
                            "traditional opinion among the ", tolower(scope),
                            " population prior to and since the year 2000 ",
                            "are equal. ", ciNote)
  }

  stmt$detail <- paste0("the observed frequency of traditional opinion ",
                        tolower(g1), " in the ", tolower(scope), " was ",
                        prettyNum(round(f1, 0), big.mark = ",") ,
                        " out of a total of ", prettyNum(t1, big.mark = ","),
                        " observations, yielding a proportion equal to ",
                        round(p1, 2), ". The observed frequency of traditional ",
                        " opinion ", tolower(g2), " in the ", tolower(scope),
                        " population was ", prettyNum(round(f2, 0), big.mark = ",") ,
                        " out of a total of ", prettyNum(t2, big.mark = ","),
                        " observations, yielding a proportion equal to ",
                        round(p2, 2), ". The difference in observed proportions ",
                        "of traditional opinion was ", round(p1 - p2, 2),
                        ", raising a z-score of ", round(zScore, 2), ".",
                        " The probability of encountering a difference in ",
                        "proportions this extreme (p-value) is approximately ",
                        round(t$p.value, 4), ". ")

  result <- list(
    df = as.data.frame(z, row.names = NULL, stringsAsFactors = FALSE),
    stmt = stmt
  )


  return(result)
}
