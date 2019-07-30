sr <- survreg(Surv(time, status)~ 1, dist="weibull")

#standardized residuals
linFit <- predict(sr, type="lp")
sderr <- (log(time)-linFit)/sr$scale

#Cox-Snell residuals
#are defined as: -log(S[t]), where S[t] is the survival function of the specified distribution
CoxSnellResidual <- function (standRes, weight=1, dist){
  standRes <- standRes[rep(seq_len(length(standRes)), weight)]
  if (dist=="lognormal") {csr <- -log(1-pnorm(standRes))}
  else if (dist=="weibull") {csr <- -log(exp(-exp(standRes)))}
}

#note: use the same weights as those employed in the survreg function
cxsn <- CoxSnellResidual(standRes=sderr, dist="weibull")

#Martingale residuals
#Status: 1=death/failure, 0=alive/censored
MartingaleResidual <- function (CoxSnellResid, Status, weight=1) {
  delta <- Status[rep(seq_len(length(Status)), weight)]
  martingale <- delta-CoxSnellResid
  data.frame(Martingale=martingale, Status=delta)
}

#note: use the same weights as those employed in the survreg function
mgale <- MartingaleResidual(CoxSnellResid=cxsn, Status=status)

#Martingale residuals range from +1 to -inf.
max(mgale$Martingale)
#for a Weibull model these residuals sum to zero (Collett, p.235)
sum(mgale$Martingale)

#the Martingale residuals plots for x1 and x2 detect the correct
#functional form, but have the wrong (opposite) sign
#for instance, the slope of the plot for x1 is negative, whereas the
#coefficient for x1 in the Weibull model is positive
plotMartingale <- function (covariate, martingaleResidual,
                            nameCovariate="covariate", weight=1) {
  cov <- covariate[rep(seq_len(length(covariate)), weight)]
  plot(cov, martingaleResidual$Martingale, pch=martingaleResidual$Status,
       xlab=nameCovariate, ylab="Martingale Residual")
  abline(h=0, lty=2, col="gray40")
  lines(lowess(cov, martingaleResidual$Martingale, iter=0), col="blue")
  legend("bottomleft", pch=c(1,0), legend=c("event","censored"))
}

#note: use the same weights as those employed in the survreg function
plotMartingale(covariate=x1, nameCovariate="x1", martingaleResidual=mgale)
plotMartingale(covariate=x2, nameCovariate="x2", martingaleResidual=mgale)
