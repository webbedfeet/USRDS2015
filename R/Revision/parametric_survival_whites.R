# Developing parametric survival models for discontinuation and 
# transplant outcomes for the white subgroup

## UPDATE: We'll do simulation modeling within each age group

abhiR::reload()
dropdir <- path(find_dropbox(), 'NIAMS','Ward','USRDS2015','data')

analytic_whites <- read_fst(path(dropdir, 'Analytic_Whites.fst'))
analytic_whites_byagegrp <- split(analytic_whites, analytic_whites$AGEGRP)

model_template <- function(dat){
  require(rms)
  m1 <- survreg(Surv(surv_time + 0.1, cens_type==3) ~ 
              SEX + factor(REGION) + rcs(zscore) + 
              ESRD_Cause +  
              Cancer + Cardia + Cva + Hyper + Ihd + Pulmon + Pvasc + Smoke + 
              DIABETES + ALCOH + DRUG + BMI2, 
            data = dat, dist='weibull')
  return(m1)
}

model_template2 <- function(dat){
  require(rms)
  m1 <- coxph(Surv(surv_time + 0.1, cens_type==3) ~ 
                  SEX + factor(REGION) + rcs(zscore) + 
                  ESRD_Cause +  
                  Cancer + Cardia + Cva + Hyper + Ihd + Pulmon + Pvasc + Smoke + 
                  DIABETES + ALCOH + DRUG + BMI2, 
                data = dat)
  return(m1)
}


whites_models <- map(analytic_whites_byagegrp, model_template2)


CoxSnell <- function(m){
  times <- as.matrix(m$y)[,1]
  status <- as.matrix(m$y)[,2]
  cs <- -log(1-pweibull(times, shape = 1/m$scale, 
                        scale = exp(predict(m, type = 'lp'))))
  plot(survfit(Surv(cs, status)~1))
  lines(qexp(ppoints(length(cs))), 1- ppoints(length(cs)), col='red')
  return(cs)
}

CoxSnellPH <- function(m){
  status <- as.matrix(m$y)[,2]
  cs = status - resid(m, type='martingale')
  plot(survfit(Surv(cs, status)~1))
  lines(qexp(ppoints(length(cs))), 1-ppoints(length(cs)), col='red')
  return(cs)
  
}
