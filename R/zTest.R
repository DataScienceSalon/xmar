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
#' @param order The order in which the levels of the explanatory variable should be analyzed. Defaults to order in which factor variables are defined.
#' @param alternative direction of the alternative hypothesis; "less","greater", or "two.sided"
#' @param conf Desired confidence level
#' @param alpha Probability of a type I error
#'
#' @return Data frame containing the results of the z-test
#'
#' @family xmar functions
#' @export#'
zTest <- function(data, success = NULL, order = NULL, alternative = "two.sided",
                  conf = 0.95, alpha = 0.05) {

  #---------------------------------------------------------------------------#
  #                                 zTest                                     #
  #---------------------------------------------------------------------------#

  # Validate data to have only two levels each
  if (length(unique(data[[1]])) != 2) stop("zTest can only compare two x two tables.")
  if (length(unique(data[[2]])) != 2) stop("zTest can only compare two x two tables.")


  # Reorder explanatory factor variable
  factorLevels <- levels(data[[2]])
  if (is.null(order)) order <- factorLevels
  if (length(intersect(order, factorLevels)) != length(factorLevels)) {
    stop("Order parameter must include all levels.")
  }
  data[[2]] <- factor(data[[2]], levels = order)

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
  ci[1] <- Diff - zScore * se
  ci[2] <- Diff + zScore * se
  pValue <- 1 - pnorm(Diff, 0, sd = sePooled)

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
    p1 = dataProp[1],
    p2 = dataProp[3],
    Diff = abs(p1 - p2),
    zScore = zScore,
    pValue = pValue,
    LowerCi = ci[1],
    UpperCi = ci[2]
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

  plot



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

  if (pValue < (alpha)) {
    stmt$conclude <- paste0("Therefore, the null hypothesis was rejected in favor of the alternative hypothesis, with ",
           conf * 100, "% confidence, that the true population proportion ",
           "of '", levels(data[[1]])[1], "' opinion in the ", levels(data[[2]])[1], " group is ", alt,
           " the true population proportion of '", levels(data[[1]])[1],
           "' opinion in the ", levels(data[[2]])[2], " group. ")
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
                        "' opinion was ", Diff, ", raising a z-score of ", zScore, ".",
                        " The probability of encountering a difference in proportions ",
                        " this extreme (p-value) is ", pValue, ". ")

  result <- list(
    z = as.data.frame(z),
    plot = plot,
    stmt = stmt
  )


  return(result)
}
