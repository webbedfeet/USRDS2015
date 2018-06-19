cox_snell <- function(mod, dat) {
  cs <- -log(1 - pweibull(dat$time_from_event, shape = 1/mod$scale,
                          scale = exp(predict(mod, dat, type='lp'))))
  plot(survfit(Surv(cs, dat$cens_type==3)~1))
  lines(qexp(ppoints(length(cs))), 1-ppoints(length(cs)), col='red')
  return(cs)
}