CoxSnell <- function(mod){
  UseMethod("CoxSnell", mod)
}

CoxSnell.psm <- function(mod){
  require(rms)
  if(!is(mod, 'psm')) stop("Model needs to be run using rms::psm")
  if(length(mod$y)==0) stop('Model needs to be run with option y=TRUE')
  cs <- -log(Survival(mod)(mod$y[,1], mod$linear.predictor, mod$scale))
  return(Surv(cs, mod$y[,2]))
}

cox_snell <- function(mod, dat) {
  cs <- -log(1 - pweibull(dat$time_from_event, shape = 1/mod$scale,
                          scale = exp(predict(mod, dat, type='lp'))))
  plot(survfit(Surv(cs, dat$cens_type==3)~1))
  lines(qexp(ppoints(length(cs))), 1-ppoints(length(cs)), col='red')
  return(cs)
}
