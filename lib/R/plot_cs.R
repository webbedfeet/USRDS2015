#' Plotting Cox-Snell residuals using ggplot2
#'
#' @param cs Cox-Snell residuals computed using the CoxSnell package
#'
#'
#' @return ggplot-based plot of Cox-Snell residuals in the form of cumulative incidence plots
#' @export
#'
#' @examples
plot_cs <- function(cs){
  require(rms)
  require(broom)
  d <- tidy(npsurv(cs~1))
  plt <- ggplot(d, aes(time, -log(estimate)))+geom_line()+#geom_point(size=1) +
    geom_abline(color='red') +
    labs(x = 'Cox-Snell Residuals', y = "Cumulative Hazard")
  return(plt)
}
