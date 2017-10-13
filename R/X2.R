#==============================================================================#
#                                     X2                                       #
#==============================================================================#
#' X2
#'
#' \code{X2} Conducts a chi-square test of independency on a contingency table
#' and renders a data frame containing the results and a plot showing the
#' chi-square distribution with reject region and observed test statistic.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#'
#' @param data Contingency table object containing observed values for chi-square hypothesis test
#' @param target Character string indicating the name of target or study variable
#' @param group Character string indicating the name of grouping variable
#' @param p Alpha .the probability of a type 1 error
#'
#' @family xmar functions
#' @export
#'
X2 <- function(data, target, group, p = 0.05) {

  x2 <- chisq.test(data)

  # Extract Chisq Data
  df <- x2$parameter
  criticalVal <- qchisq(p, df, lower.tail = F)

  # Create Table
  table <- data.frame(Test = "Chi-square Test of Independence",
                      Target = target,
                      Group = group,
                      Df = x2$parameter,
                      X2_Critical = qchisq(p, df, lower.tail = F),
                      X2_Observed = x2$statistic,
                      p_Value = x2$p.value,
                      Decision = ifelse(x2$p.value >= p,"Fail to Reject", "Reject"),
                      row.names = NULL)

  # Format Plot Data
  gg   <- data.frame(x=seq(0,1.2 * x2$statistic,0.1))
  gg$y <- dchisq(gg$x,df)
  ggNotReject <- subset(gg, x <= criticalVal)
  ggReject <- subset(gg, x > criticalVal)
  r1Desc <- data.frame(Result = rep("Fail to Reject", nrow(ggNotReject)))
  r2Desc <- data.frame(Result = rep("Reject", nrow(ggReject)))
  r1 <- cbind(r1Desc, ggNotReject)
  r2 <- cbind(r2Desc, ggReject)
  gg <- rbind(r1,r2)

  # Format Mark of observed Statistic
  observed <- paste("observed X2 = ", round(x2$statistic, 2))
  observedx <- x2$statistic
  observedy <- min(0.01, max(10 * dchisq(x2$statistic, df), 0.01))
  observedx1 <- x2$statistic
  observedx2 <- x2$statistic
  observedy1 <- observedy * .7
  observedy2 <- 0

  # Format Mark of observed Statistic
  reject <- paste("Critical X2 = ", round(criticalVal, 2))
  rejectx <- criticalVal
  rejecty <- dchisq(criticalVal, df) + .01
  rejectx1 <- criticalVal
  rejectx2 <- criticalVal
  rejecty1 <- rejecty * .9
  rejecty2 <- dchisq(criticalVal, df) * 1.1

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
    ggplot2::labs(title = paste("Chi-square Test of Independence of", target, "and", group),                  x = "X2",
                  y = "Probability")


  x2 <- list(
    table = table,
    plot = plot
  )
  return(x2)
}
