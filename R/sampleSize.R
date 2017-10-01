#==============================================================================#
#                               sampleSize                                     #
#==============================================================================#
#' sampleSize
#'
#' \code{sampleSize} Performs a sampleSize analysis for several null proportions based
#' upon the number of levels in each grouping variable.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family xmar functions
#' @export
#'
sampleSize <- function(xmar) {

  #---------------------------------------------------------------------------#
  #                            Obtain Parameters                              #
  #---------------------------------------------------------------------------#

  proportions <- list(
    Region = 1 / length(levels(xmar$Region)),
    Degree = 1 / length(levels(xmar$Degree)),
    Views = 1 / length(levels(xmar$Views)),
    Class = 1 / length(levels(xmar$Class)),
    Attitude = 1 / length(levels(xmar$Attitude)),
    Stray = 1 / length(levels(xmar$Stray))
  )

  me <- 0.05
  zstar <- qnorm(.975)

  sampleSizes <- rbindlist(lapply(seq_along(proportions), function(p) {
    ss <- zstar^2 * proportions[[p]] * (1 - proportions[[p]]) / me^2
    ssl = list(
     `Grouping Variable` = names(proportions[p]),
     Levels = 1 / proportions[[p]],
      p = round(proportions[[p]],2),
      zStar = round(zstar,2),
      `Margin of Error` = me,
      `Sample Size` = ceiling(ss)
    )
    ssl
  }))

  return(sampleSizes)
}
