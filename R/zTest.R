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
#' @param xOrder The order in which the levels of the explanatory variable should be analyzed. Defaults to order in which factor variables are defined.
#' @param alternative direction of the alternative hypothesis; "less","greater", or "two.sided"
#' @param conf Desired confidence level
#' @param alpha Probability of a type I error
#'
#' @return Data frame containing the results of the z-test
#'
#' @family xmar functions
#' @export
zTest <- function(data, success = NULL, xOrder = NULL, alternative = "two.sided",
                  conf = 0.95, alpha = 0.05) {

  #---------------------------------------------------------------------------#
  #                                 zTest                                     #
  #---------------------------------------------------------------------------#

  # Validate data to have only two levels each
  if (length(unique(data[[1]])) != 2) stop("zTest can only compare two x two tables.")
  if (length(unique(data[[2]])) != 2) stop("zTest can only compare two x two tables.")


  # Reorder explanatory factor variable
  factorLevels <- levels(data[[2]])
  if (is.null(xOrder)) xOrder <- factorLevels
  if (length(intersect(xOrder, factorLevels)) != length(factorLevels)) {
    stop("Order parameter must include all levels.")
  }
  data[[2]] <- factor(data[[2]], levels = xOrder)

  # Reorder response factor variable
  factorLevels <- levels(data[[1]])
  if (!is.null(success)) {
    if (!(success %in% factorLevels)) {
      stop(paste0("Invalid 'success' value provided. ",
                  "Valid factor levels are '", factorLevels[1],
                  "' and '", factorLevels[2]), "'.")
    } else {
      if (success == factorLevels[2]) {
        data[[1]] <- factor(data[[1]], levels = c(factorLevels[2], factorLevels[1]))
      }
    }
  }

  # Extract Variables
  dataTbl <- table(data)
  dataFreq <- margin.table(dataTbl, 1)
  dataProp <- prop.table(dataTbl, 2)
  p1 <- dataProp[1]
  p2 <- dataProp[3]
  n1 <- dataFreq[1]
  n2 <- dataFreq[2]
  f1 <- dataTbl[1]
  f2 <- dataTbl[3]

  # Compute pooled proportion, standard error and z-score
  p <- ((p1 * n1) + (p2 * n2)) / (n1 + n2)
  se <- sqrt( ((p1 * (1 - p1)) / n1) + ((p2 * (1 - p2)) / n2))
  sePooled <- sqrt( ((p * (1 - p)) / n1) + ((p * (1 - p) / n2)) )
  Diff <-  abs(p1 - p2)
  ttl <- sum(dataTbl)
  zScore <- abs(Diff - 0) / sePooled
  ci <- numeric(2)
  ci[1] <- Diff - (zScore * se)
  ci[2] <- Diff + (zScore * se)
  pValue <- 1 - pnorm(Diff, 0, sd = sePooled)
  topGroup <- ifelse(p1 > p2, levels(data[[2]])[1], levels(data[[2]])[2])
  bottomGroup <- ifelse(p1 > p2, levels(data[[2]])[2], levels(data[[2]])[1])
  if (p1 > p2) {
    rr <- epitools::riskratio(x = t(dataTbl), method = "wald", conf.level = conf)
  } else {
    rr <- epitools::riskratio(x = t(dataTbl), method = "wald", conf.level = conf, rev = "columns")
  }

  z <- list(
    Test = ifelse(alternative == "less", "One-Sided, Two-Sample Proportion Test",
                   ifelse(alternative == "greater", "One-Sided, Two-Sample Proportion Test",
                          "Two-Sample Proportion Test")),
    Proportion = paste("Opinion = ", levels(data[[1]])[1]),
    Groups = paste0(levels(data[[2]])[1], "/", levels(data[[2]])[2]),
    H0 = paste0("H0: p1 = p2"),
    Ha = ifelse(alternative == "less", "p1 < p2",
                ifelse(alternative == "greater",  "p1 > p2", "p1 <> p2")),
    Alpha = alpha,
    p1 = as.numeric(round(dataProp[1], 2)),
    p2 = as.numeric(round(dataProp[3], 2)),
    Diff = as.numeric(round(abs(p1 - p2), 2)),
    zScore = as.numeric(round(zScore, 4)),
    pValue = as.numeric(round(pValue, 4)),
    LowerCi = as.numeric(round(ci[1], 4)),
    UpperCi = as.numeric(round(ci[2], 4)),
    Decision = ifelse(pValue < alpha, "Reject", "Fail to Reject")
  )

  #---------------------------------------------------------------------------#
  #                                  Plot                                     #
  #---------------------------------------------------------------------------#
  area2sd <- round(pnorm(2) - pnorm(-2), 3)
  twosd <- function(x) {
    two <- dnorm(x, mean = 0, sd = se)
    two[x < -2*se | x > 2*se] <- NA
    return(two)
  }
  plot <- ggplot2::ggplot(data = data.frame(x = c(-4*se,4*se)),
                          ggplot2::aes(x = x)) +
    ggplot2::theme_minimal() +
    ggplot2::stat_function(fun = dnorm) +
    ggplot2::stat_function(fun = twosd, geom = "area", fill = "green", alpha = 0.2) +
    ggplot2::geom_vline(xintercept = z$Diff) +
    ggplot2::geom_text(x = 0,  y = 35, size = 4, fontface = "bold",
                       label = paste0(area2sd * 100, "%")) +
    ggplot2::geom_text(x = z$Diff, y = 10, size = 4, fontface = "bold",
                       label = paste0("Observed \nDifference")) +
    ggplot2::theme(legend.position="bottom",
                   text=ggplot2::element_text(family="Open Sans")) +
    ggplot2::geom_point(data = data.frame(Diff = z$Diff),
                        ggplot2::aes(x = Diff, y = 0), size = 2, color = "red") +
    ggplot2::labs(title = "Difference in Proportion of Traditional Opinion vis-a-vis Normal Distribution",
                  x = "Difference in Proportion")

  #---------------------------------------------------------------------------#
  #                                 Footnotes                                 #
  #---------------------------------------------------------------------------#

  footnote <- list()
  footnote[[1]] <-        "       Test: The hypothesis test being conducted."
  footnote[[2]] <- paste0(" Proportion: The proportion being analyzed.")
  footnote[[3]] <- paste0("     Groups: The groups being analyzed.")
  footnote[[4]] <- paste0("         H0: The null hypothesis.")
  footnote[[5]] <- paste0("         Ha: The alternative hypothesis.")
  footnote[[6]] <- paste0("      Alpha: The acceptable probability of a type I error.")
  footnote[[7]] <- paste0("         p1: The proportion of the male population with traditional opinions.")
  footnote[[8]] <- paste0("         p2: The proportion of the female population with traditional opinions.")
  footnote[[9]] <-  paste0("       Diff: The difference in proportions (p1 - p2).")
  footnote[[10]] <- paste0("     zScore: The z-score associated with the difference in proportions.")
  footnote[[11]] <- paste0("     pValue: The probability of encountering a zScore as extreme as that observed.")
  footnote[[12]] <- paste0("    LowerCI: The lower 95% confidence interval for the difference in proportions.")
  footnote[[13]] <- paste0("    UpperCI: The upper 95% confidence interval for the difference in proportions.")
  footnote[[14]] <- paste0("   Decision: The decision w.r.t. the null hypothesis.")


  #---------------------------------------------------------------------------#
  #                         Statement of Results                              #
  #---------------------------------------------------------------------------#

  # Format Statements
  alt <- ifelse(alternative == "two.sided", "not equal to",
                ifelse(alternative == "less", "less than","greater than"))

  stmt <- list()
  stmt$type <- paste0("This was a two-proportion z-test of the null hypothesis that the true population proportions of '",
                      levels(data[[1]])[1], "' opinion among the ", levels(data[[2]])[1], " and ",
                      levels(data[[2]])[2], " groups are equal with ", (conf * 100), "% confidence.")

  if ((alternative == "two.sided" & pValue < (alpha / 2))
       | (alternative != "two.sided" & pValue < alpha)) {
    stmt$conclude <- paste0("Therefore, the null hypothesis was rejected in favor of the alternative hypothesis, with ",
           conf * 100, "% confidence, that the true population proportion ",
           "of '", levels(data[[1]])[1], "' opinion in the ", levels(data[[2]])[1], " group is ", alt,
           " the true population proportion of '", levels(data[[1]])[1],
           "' opinion in the ", levels(data[[2]])[2], " group. Further, ",
           "one can conclude with 95% coinfidence that the ", topGroup, " group is ",
           round(rr$measure[[2]], 2), " times as likely as the ", bottomGroup, " group to hold traditional opinions.")
  } else {
    stmt$conclude <- paste0("Therefore, the null hypothesis was not rejected; consequently, one concludes, with ",
           conf * 100, "% confidence, that the true proportions of '", levels(data[[1]])[1],
           "' opinion among the ", levels(data[[2]])[1], " and ",
           levels(data[[2]])[2], " groups are equal.")
  }

  stmt$detail <- paste0("the observed frequency of '", levels(data[[1]])[1], "' opinion in the ",
                        levels(data[[2]])[1], " group was ",
                        prettyNum(round(f1, 0), big.mark = ",") ,
                        " out of a total of ", prettyNum(n1, big.mark = ","),
                        " observations, yielding a proportion equal to ", round(p1, 2), ".",
                        "The observed frequency of '", levels(data[[1]])[2], "' opinion was ",
                        prettyNum(round(f2, 0), big.mark = ",") ,
                        " out of a total of ", prettyNum(n2, big.mark = ","),
                        " observations, yielding a proportion equal to ", round(p2, 2), ".",
                        " The difference in observed proportions of'", levels(data[[1]])[2],
                        "' opinion was ", round(Diff, 2), ", raising a z-score of ", round(zScore, 2), ".",
                        " The probability of encountering a difference in proportions",
                        " this extreme (p-value) is approximately ", round(pValue, 4), ". ")

  result <- list(
    df = as.data.frame(z, row.names = NULL, stringsAsFactors = FALSE),
    plot = plot,
    footnote = footnote,
    stmt = stmt
  )


  return(result)
}
