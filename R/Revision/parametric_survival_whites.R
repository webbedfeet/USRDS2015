# Developing parametric survival models for discontinuation and 
# transplant outcomes for the white subgroup

## UPDATE: We'll do simulation modeling within each age group

abhiR::reload()
dropdir <- path(find_dropbox(), 'NIAMS','Ward','USRDS2015','data')

analytic_whites <- read_fst(path(dropdir, 'Analytic_Whites.fst')) %>% 
  mutate(REGION = factor(REGION))
analytic_whites_byagegrp <- split(analytic_whites, analytic_whites$AGEGRP)

model_template <- function(dat, dist = 'weibull'){
  require(rms)
  m1 <- survreg(Surv(surv_time + 0.01, cens_type==3) ~ 
                  factor(REGION) + rcs(zscore) + 
                  SEX*(ESRD_Cause +  BMI2) + 
                  Cancer + Cardia + Cva + Hyper + Ihd + Pulmon + Pvasc + Smoke + 
                  DIABETES + ALCOH + DRUG + BMI2, 
                data = dat, dist=dist)
  return(m1)
}

model_template_rms <- function(dat, dist = 'weibull'){
  require(rms)
  m1 <- psm(Surv(surv_time + 0.01, cens_type==3) ~ 
                  REGION + SEX*rcs(zscore) + 
                  SEX*(ESRD_Cause +  BMI2) + 
                  Cancer + Cardia + Cva + Hyper + Ihd + Pulmon + Pvasc + Smoke + 
                  DIABETES + ALCOH + DRUG + BMI2, 
                data = dat, dist=dist, y = T, x = T)
  return(m1)
}


whites_models_weibull <- map(analytic_whites_byagegrp, model_template_rms)
whites_models_loglogistic <- map(analytic_whites_byagegrp, model_template_rms, dist = 'loglogistic')
whites_models_lognormal <- map(analytic_whites_byagegrp, model_template_rms, dist = 'lognormal')

CoxSnell.psm <- function(m, plot = F){
  require(rms)
  if(!is(m, 'psm')) stop("Model needs to be run using rms::psm")
  r <- residuals(m)
  cs <- -log(attr(r,'theoretical')(exp(r[,1]))) # exp for weibull, loglogistic and lognormal
  if(plot){
    survd <- broom::tidy(survfit(Surv(cs, r[,2])~1)) # Compute survival curve of censored CS resid
    ggplot(survd, aes(time, -log(estimate)))+geom_point() + geom_abline(color = 'red')
  }
  return(Surv(cs, r[,2]))
}

cs_weibull <- map(whites_models_weibull, CoxSnell.psm)
cs_loglogistic <- map(whites_models_loglogistic, CoxSnell.psm)
cs_lognormal <- map(whites_models_lognormal, CoxSnell.psm)

plot_cs <- function(cs){
  require(rms)
  require(broom)
  d <- tidy(npsurv(cs~1))
  plt <- ggplot(d, aes(time, -log(estimate)))+geom_point() + geom_abline(color='red')
  
}
