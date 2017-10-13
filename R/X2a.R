#==============================================================================#
#                                     X2a                                      #
#==============================================================================#
#' X2a
#'
#' \code{X2a} Conducts a chi-square test of independency on a contingency table
#' and renders a data frame containing the results and a plot showing the
#' chi-square distribution with reject region and observed test statistic.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#'
#' @param x2aTable Contingency table object containing observed values for chi-square hypothesis test
#' @param target Character string indicating the name of target or study variable
#' @param group Character string indicating the name of grouping variable
#' @param p Alpha .the probability of a type 1 error
#'
#' @family xmar functions
#' @export
#'
X2a <- function(x2aTable, target, group, p = 0.05) {

  x2a <- chisq.test(x2aTable)

  # Extract Chisq Data
  df <- x2a$parameter
  criticalVal <- qchisq(p, df, lower.tail = F)

  # Create Table
  table <- data.frame(Test = "Chi-square Test of Independence",
                      Target = target,
                      Group = group,
                      Df = x2a$parameter,
                      X2a_Critical = qchisq(p, df, lower.tail = F),
                      X2a_Observed = x2a$statistic,
                      p_Value = x2a$p.value,
                      Decision = ifelse(x2a$p.value >= p,"Fail to Reject", "Reject"),
                      row.names = NULL)

  # Format Plot Data
  gg   <- data.frame(x=seq(0,1.2 * x2a$statistic,0.1))
  gg$y <- dchisq(gg$x,df)
  ggNotReject <- subset(gg, x <= criticalVal)
  ggReject <- subset(gg, x > criticalVal)
  r1Desc <- data.frame(Result = rep("Fail to Reject", nrow(ggNotReject)))
  r2Desc <- data.frame(Result = rep("Reject", nrow(ggReject)))
  r1 <- cbind(r1Desc, ggNotReject)
  r2 <- cbind(r2Desc, ggReject)
  gg <- rbind(r1,r2)

  # Format Mark of observed Statistic
  observed <- paste("observed X2a = ", round(x2a$statistic, 2))
  observedx <- x2a$statistic
  observedy <- min(0.01, max(10 * dchisq(x2a$statistic, df), 0.01))
  observedx1 <- x2a$statistic
  observedx2a <- x2a$statistic
  observedy1 <- observedy * .7
  observedy2 <- 0

  # Format Mark of observed Statistic
  reject <- paste("Critical X2a = ", round(criticalVal, 2))
  rejectx <- criticalVal
  rejecty <- dchisq(criticalVal, df) + .01
  rejectx1 <- criticalVal
  rejectx2a <- criticalVal
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
    ggplot2::geom_segment(ggplot2::aes(x = observedx1, y = observedy1, xend = observedx2a, yend = observedy2)) +
    ggplot2::geom_segment(ggplot2::aes(x = rejectx1, y = rejecty1, xend = rejectx2a, yend = rejecty2)) +
    ggplot2::labs(title = paste("Chi-square Test of Independence of", target, "and", group),
                  x = "X2a",
                  y = "Probability")

  # Format Contingency Observed Tables
  d <- as.data.frame(ftable(x2a$observed))
  d1 <- d$Opinion[1:3]
  d2 <- d$Freq[1:3]
  d3 <- d$Freq[4:6]
  dfof <- data.frame(Opinion = d1,
                   Yes = d2,
                   No = d3,
                   Total = d2 + d3)
  dTtl <- data.frame(Opinion = "Total",
                   Yes = sum(d2),
                   No = sum(d3),
                   Total = sum(dfof$Total))
  dfof <- rbind(dfof, dTtl)
  dfop <- dfof %>% mutate(Yes = round(Yes / Total, 2),
                          No = round(No / Total, 2),
                          Total = 1)

  # Format Contingency Expected Tables
  d <- as.data.frame(ftable(x2a$expected))
  d1 <- d$Opinion[1:3]
  d2 <- d$Freq[1:3]
  d3 <- d$Freq[4:6]
  dfef <- data.frame(Opinion = d1,
                     Yes = d2,
                     No = d3,
                     Total = d2 + d3)
  dTtl <- data.frame(Opinion = "Total",
                     Yes = sum(d2),
                     No = sum(d3),
                     Total = sum(dfef$Total))
  dfef <- rbind(dfef, dTtl)
  dfep <- dfef %>% mutate(Yes = round(Yes / Total, 2),
                          No = round(No / Total, 2),
                          Total = 1)


  x2a <- list(
    table = table,
    obsFreq = dfof,
    obsProp = dfop,
    expFreq = dfef,
    expProp = dfep,
    plot = plot
  )
  return(x2a)
}
