#==============================================================================#
#                                 zTest                                        #
#==============================================================================#
#' zTest
#'
#' \code{zTest} Conducts a z-tests for a multi-level categorical explanatory
#' variable on a response variable. Returns data and plots of the z-distribution
#' vis-a-vis the observed test statistic.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#'
#' @param data data frame containing data to be analyzed
#' @param y Chracter vector containing the name of the response variable
#' @param x Chracter vector containing the name of the explanatory variable
#' @param order when x is given, order of levels of x in which to subtract parameters
#' @param alternative direction of the alternative hypothesis; "less","greater", or "twosided"
#' @param conf confidence level, value between 0 and 1
#'
#' @return Data frame containing the results of the z-test
#'
#' @family xmar functions
#' @export
#'
zTest <- function(data, y, x, order = NULL, alternative = "twosided",
                  success = "Liberal", conf = 0.95) {

  # Reorder factors
  factorLevels <- levels(data[[1]])
  if (is.null(order)) order <- factorLevels
  if (length(intersect(order, factorLevels)) != length(factorLevels)) {
    stop("Order parameter must include all levels.")
  }
  data[[1]] <- factor(data[[1]], levels = order)

  # Extract proportions and sample sizes
  p <- data %>% filter(.[[2]] == success) %>% select(Prop)
  n <- data %>% filter(.[[2]] == success) %>% select(Ttl)

  # Create the iteration vector
  groups <- lapply(seq_along(order[1:length(order)-1]), function(g) {
    l <- list()
    l$pair <- paste(order[g], 'and', order[g+1])
    l$p1Name <- order[g]
    l$p2Name <- order[g+1]
    l$p1 <- p$Prop[g]
    l$p2 <- p$Prop[g+1]
    l$n1 <- n$Ttl[g]
    l$n2 <- n$Ttl[g+1]
    l
  })

  # Create data frame and statements
  z <- lapply(seq_along(groups), function(g) {
    df <- list()
    stmt <- list()

    # Obtain data and conduct test
    numSuccess <- (data %>%
                     filter(.[[1]] %in% c(groups[[g]]$p1Name, groups[[g]]$p1Name) &
                              .[[2]] == success) %>% select(Freq))$Freq
    numTotal <- (data %>%
                   filter(.[[1]] %in% c(groups[[g]]$p1Name, groups[[g]]$p1Name) &
                            .[[2]] == success) %>% select(Ttl))$Ttl
    t <- prop.test(numSuccess, numTotal, correct = FALSE,
                   alternative = "greater", conf.level = conf)

    # Format Data Frame
    df$Group <- groups[[g]]$pair
    df$p1 <- groups[[g]]$p1Name
    df$p2 <- groups[[g]]$p2Name
    df$null <- ifelse(alternative == "two-sided", "p1 - p2 = 0",
                          ifelse(alternative == "less", "p1 - p2 => 0",
                                 "p1 - p2 <= 0"))
    df$alt <- ifelse(alternative == "two-sided", "p1 - p2 <> 0",
                         ifelse(alternative == "less", "p1 - p2 < 0",
                                "p1 - p2 > 0"))
    df$X2 <- t$statistic
    df$CI <- paste0("[", round(t$conf.int[1], 4), " ",
                                   round(t$conf.int[2], 4), "]")
    df$pValue <- ifelse(t$p.value < (1-conf), "p < .05",
                            round(t$p.value, 2))
    df$p1Est <- ifelse(t$estimate[1] <- 0.001, round(t$estimate[1], 4),
                           ifelse(t$estimate[1] <- 0.1, round(t$estimate[1], 3),
                                  ifelse(t$estimate[1] <- 10, round(t$estimate[1], 2),
                                         ifelse(t$estimate[1] <- 100, round(t$estimate[1], 1),
                                                round(t$estimate[1], 0)))))
    df$p2Est <- ifelse(t$estimate[2] <- 0.001, round(t$estimate[2], 4),
                           ifelse(t$estimate[2] <- 0.1, round(t$estimate[2], 3),
                                  ifelse(t$estimate[2] <- 10, round(t$estimate[2], 2),
                                         ifelse(t$estimate[2] <- 100, round(t$estimate[2], 1),
                                                round(t$estimate[2], 0)))))

    # Format Statements
    null <- ifelse(alternative == "two-sided", "not equal to",
                   ifelse(alternative == "less", "greater than or equal to",
                          "less than or equal to"))
    stmt$type <- if (alternative == "two.sided") {
      "This was a two-proportion z-test of the null hypothesis that the true population proportions, p1, and p2 are equal. "
    } else {
      paste0("This was a two-sample proportion test of the null hypothesis that the true population proportion of liberal ",
             "opinion (p1) in the ", groups[[g]]$p1Name, " group, is ", null, " the true population proportion of liberal opinion (p2) ",
             "in the ", groups[[g]]$p2Name, " group. ")
    }

    stmt$conclude <- if (t$p.value < (1-conf)) {
      if (alternative == "two.sided") {
        paste0("Therefore, the null hypothesis was rejected in favor of the alternative hypothesis, with ",
               conf * 100, "% confidence, that the true population proportion ",
               "of liberal opinion in the ", groups[[g]]$p1Name, " group is ", alternative,
               " the true population proportion of ",
               "liberal opinion in the ", groups[[g]]$p2Name, " group. ")
      } else {
        paste0("Therefore, the null hypothesis was rejected in favor of the alternative hypothesis that the true population proportion ",
               "of liberal opinion in the ", groups[[g]]$p1Name, " group is ", alternative, " than the true population proportion of ",
               "liberal opinion in the ", groups[[g]]$p2Name, " group. ")

      }
    } else {
      if (alternative == "two.sided") {
        paste0("Therefore, the null hypothesis was not rejected; consequently, one concludes, with ",
               conf * 100, "% confidence, that the true proportions of liberal opinion ",
               "in the ", groups[[g]]$p1Name, " group is ", null, " the true population proportion of ",
               "liberal opinion in the ", groups[[g]]$p2Name, " group. ")
      } else {
        paste0("Therefore, the null hypothesis was not rejected; consequently, one concludes, with ",
               conf * 100, "% confidence, that the true proportions of liberal opinion ",
               "in the ", groups[[g]]$p1Name, " group is ", null, " the true population proportion of ",
               "liberal opinion in the ", groups[[g]]$p2Name, " group, ")
      }
    }

    stmt$detail <- if (t$p.value < (1-conf)) {
      paste0("The observed frequency of liberal opinion for the ", groups[[g]]$p1Name, " group was ",
             prettyNum(round(t$estimate[1] * groups[[g]]$n1, 0), big.mark = ",") ,
             " out of a total of ", prettyNum(groups[[g]]$n1, big.mark = ","),
             " observations, yielding a proportion equal to ", round(groups[[g]]$p1, 2), ".",
             "The observed frequency of liberal opinion for the ", groups[[g]]$p2Name, " group was ",
             prettyNum(round(t$estimate[2] * groups[[g]]$n2, 0), big.mark = ",") ,
             " out of a total of ", prettyNum(groups[[g]]$n2, big.mark = ","),
             " observations, yielding a proportion equal to ", round(groups[[g]]$p2, 2), ".",
             " The difference in observed proportions of liberal opinion was ",
             round(groups[[g]]$p1 - groups[[g]]$p2, 4), "The probability of encountering ",
             " a difference in proportions this extreme (p-value) is ", df$pValue., ". ")
    }

    res <- list(
      df = df,
      stmt = stmt
    )
    res
  })

  return(z)
}
