#==============================================================================#
#                                     analyze3d                                #
#==============================================================================#
#' analyze3d
#'
#' \code{results} Performs an analysis for a data frame containing a response
#' and an explanatory variable.  The key functions are:
#'
#' @param data Data frame containing two columns; the response and the explanatory variable.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family xmar functions
#' @export
analyze3d <- function(data2d, data3d) {

  # Conduct 2-way  Chisq Test
  data2d <- formatData(data2d)
  x22d <- X23D(data2d$raw)

  # Format 2-way data for plotting
  df2d <- as.data.frame(ftable(table(data2d$raw), 2))
  df2d <- as.data.frame(df2d %>% group_by(GenderAge) %>%
                          mutate(Ttl = sum(Freq)))
  df2d <- df2d %>% mutate(Prop = Freq / Ttl)



  # Format 3-way data for mosaic plotting
  vnames <- list(set_varnames = c(Opinion = "Opinion",
                                    Gender = "Gender",
                                    Age = "AgeGroup"))
  lnames <- list(Opinion = c("Traditional", "Non-Traditional"),
                 Gender = c("Male", "Female"),
                 Age = "15-24", "25-44", "45-64", "65+")
  x23d <- X23D(data3d)
  struct <- structable(Gender + AgeGroup  ~ Opinion, table(x23d$raw))
  mplot <- mosaic(struct, shade = TRUE, legend = TRUE,
                 labeling_args=vnames, set_labels=lnames)

  # Conduct Pairwise Comparisons
  d <- list(
    raw = data2d$raw,
    observed = data.frame(GenderAge = df2d$GenderAge,
                          Opinion = df2d$Opinion,
                          Freq = df2d$Freq,
                          Ttl = df2d$Ttl,
                          Prop = df2d$Prop)
  )
  numTests <- length(levels(data2d$raw[[2]])) - 1
  xOrder <- df2d %>% filter(Opinion != "Traditional") %>%
                            arrange(Prop) %>%
                            select(GenderAge)
  xOrder <- as.vector(xOrder$GenderAge)
  test <- propTest(d, xOrder = xOrder, alternative = "less",
                   success = "Traditional", conf = 0.95, alpha = 0.05 / numTests)


  analysis = list(
    data = list(
      raw = d$raw,
      observed = x23d$observed,
      expected = x23d$expected
    ),
    plot = mplot,
    test = test
  )
  return(analysis)
}
