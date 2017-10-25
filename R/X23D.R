#==============================================================================#
#                                   X23D                                       #
#==============================================================================#
#' X23D
#'
#' \code{X23D} Format data for three way analysis
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#'
#' @param data Data frame containing two or three columns, the values for the
#' response and explanatory variable(s). The first column is the response
#' variable.
#'
#' @return List containing a contingency table and a data frame of descriptive statistics
#'
#' @family xmar functions
#' @export
#'
X23D <- function(data) {

  # Threeway observed and expected contingency table
  observed <- ftable(table(data))
  x2 <- chisq.test(ftable(table(data)))

  # Format expected counts
  expected <- x2$expected
  o <- observed
  for (i in 1:dim(observed)[1]) {
    for (j in 1:dim(observed)[2]) {
      o[i,j] <- expected[i,j]
    }
  }
  expected <- round(o, 0)

  data = list(
    raw = data,
    observed = observed,
    expected = expected,
    test = x2
  )
  return(data)
}
