#' Martingale residuals
#'
#' @param mod An object of class `survival::coxph`, `survival::survreg`, `rms::cph` or `rms::psm`
#'
#' @return Martingale residuals
#' @export
#'
#' @examples
Martingale <- function(mod){
  UseMethod('Martingale', mod)
}

Martingale.coxph <- function(mod){
  r <- resid(mod, type='martingale')
  return(r)
}

Martingale.cph <- function(mod){
  r <- residuals(mod, type='martingale')
  return(r)
}

Martingale.survreg <- function(mod){
  cs = CoxSnell(mod)
  r <- mod$y[,2] - cs[,1]
  return(r)
}

Martingale.psm <- function(mod){
  cs <- CoxSnell(mod)
  r <- mod$y[,2] - cs[,1]
  return(r)
}
