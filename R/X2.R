#==============================================================================#
#                                     X2                                      #
#==============================================================================#
#' X2
#'
#' \code{X2} Conducts a chi-square test of independency on a contingency table
#' and renders a data frame containing the results and a plot showing the
#' chi-square distribution with reject region and observed test statistic.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#'
#' @param x2Table Contingency table object containing observed values for chi-square hypothesis test
#' @param y Character string indicating the name of the respnse variable
#' @param x Character string indicating the name of the explanatory variable
#' @param p Alpha .the probability of a type 1 error
#'
#' @family xmar functions
#' @export
#'
X2 <- function(x2Table, y, x, p = 0.05) {

  x2 <- chisq.test(x2Table)

  # Extract Chisq Data
  df <- x2$parameter
  criticalVal <- qchisq(p, df, lower.tail = F)

  # Create Table
  table <- data.frame(Test = "Chi-square Test of Independence",
                      Target = y,
                      Group = x,
                      N = sum(x2Table),
                      Df = x2$parameter,
                      X2_Critical = qchisq(p, df, lower.tail = F),
                      X2_Observed = x2$statistic,
                      p_Value = ifelse(x2$p.value < 0.05, "p < 0.05", round(x2$p.value, 2)),
                      Decision = ifelse(x2$p.value >= p,"Fail to Reject", "Reject"),
                      row.names = NULL)

  # Format Plot Data
  gg   <- data.frame(x=seq(0,max(table$X2_Critical, table$X2_Observed) * 1.2,
                           max(table$X2_Critical, table$X2_Observed) / 100))
  gg$y <- dchisq(gg$x,df)
  ggNotReject <- subset(gg, x <= criticalVal)
  ggReject <- subset(gg, x > criticalVal)
  r1Desc <- data.frame(Result = rep("Fail to Reject", nrow(ggNotReject)))
  r2Desc <- data.frame(Result = rep("Reject", nrow(ggReject)))
  r1 <- cbind(r1Desc, ggNotReject)
  r2 <- cbind(r2Desc, ggReject)
  gg <- rbind(r1,r2)

  # Format Mark of observed Statistic
  observed <- paste("observed X2 = ", ifelse(x2$statistic > 100, round(x2$statistic, 0),
                                             ifelse(x2$statistic > 10, round(x2$statistic, 1),
                                                    ifelse(x2$statistic > 0.1, round(x2$statistic, 2),
                                                           ifelse(x2$statistic > .001, round(x2$statistic, 3),
                                                                  round(x2$statistic, 4))))))
  observedx <- x2$statistic
  observedy <- 0.05
  observedx1 <- x2$statistic
  observedx2 <- x2$statistic
  observedy1 <- observedy * .7
  observedy2 <- 0

  # Format Mark of observed Statistic
  reject <- paste("Critical X2 = ", round(criticalVal, 2))
  rejectx <- criticalVal
  rejecty <- 0.05
  rejectx1 <- criticalVal
  rejectx2 <- criticalVal
  rejecty1 <- rejecty * .7
  rejecty2 <- dchisq(criticalVal, df) * 1.5

  # Render Plot
  plot <- ggplot2::ggplot(gg) +
    ggplot2::theme_minimal(base_size = 24) +
    ggplot2::scale_fill_brewer(palette = 'Greens', name = "Decision") +
    ggplot2::geom_text(x = observedx, y = observedy, label = observed, family = "Open Sans",
                       colour="black",size = 8) +
    ggplot2::geom_area(ggplot2::aes(x = x, y = y, fill = Result)) +
    ggplot2::geom_text(x = rejectx, y = rejecty, label = reject, family = "Open Sans",
                       colour="black",size = 8) +
    ggplot2::geom_segment(ggplot2::aes(x = observedx1, y = observedy1, xend = observedx2, yend = observedy2)) +
    ggplot2::geom_segment(ggplot2::aes(x = rejectx1, y = rejecty1, xend = rejectx2, yend = rejecty2)) +
    ggplot2::labs(title = paste("Chi-square Test of Independence of", y, "and", x),
                  x = "X2",
                  y = "Probability")

  stmt <- list()
  stmt1 <-

  x2 <- list(
    table = table,
    obsFreq = ftable(x2$observed),
    expFreq = round(ftable(x2$expected),0),
    plot = plot
  )
  return(x2)
}
