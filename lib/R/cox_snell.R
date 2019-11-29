#' Cox-Snell residuals of survival objects
#'
#' @param mod A survival model of class `coxph`, `rms::cph` or `rms::psm`
#'
#' @return Cox-Snell residuals with survival status, as a `Surv` object
#' @export
#'
#' @examples
CoxSnell <- function(mod){
  UseMethod("CoxSnell", mod)
}

CoxSnell.psm <- function(mod){
  require(rms)
  if(!is(mod, 'psm')) stop("Model needs to be run using rms::psm")
  if(length(mod$y)==0) stop('Model needs to be run with option y=TRUE')
  cs <- -log(Survival(mod)(times = mod$y[,1], lp = mod$linear.predictor))
  # cs <- cs + (1-mod$y[,2])*log(2)
  return(Surv(cs, mod$y[,2]))
}

CoxSnell.cph <- function(mod){
  require(rms)
  martingale_resid <- residuals(mod, type='martingale')
  cs = mod$y[,2] - martingale_resid
  return(Surv(cs, mod$y[,2]))
}

CoxSnell.coxph <- function(mod){
  martingale_resid <- resid(mod, type='martingale')
  cs <- mod$y[,2] - martingale_resid
  return(Surv(cs, mod$y[,2]))
}

CoxSnell.survreg <- function(mod){
  if(length(mod$y)==0) stop('Model needs to be run with option y=TRUE')
  stdres <- (log(mod$y[,1]) - mod$linear.predictors)/mod$scale
  surv <- case_when(
    mod$dist=='weibull' ~ exp(-exp(stdres)),
    mod$dist=='lognormal' ~ 1 - pnorm(stdres),
    mod$dist=='loglogistic' ~ 1 - plogis(stdres),
    TRUE ~ NA
  )
  if(is.na(surv)){
    stop(paste('The distribution', mod$dist, 'is currently not implemented.\n'))
  }
  cs <- -log(surv)
  return(Surv(cs, mod$y[,2]))
}
