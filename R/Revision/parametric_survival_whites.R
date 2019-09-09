# Developing parametric survival models for discontinuation and 
# transplant outcomes for the white subgroup


# Setup -------------------------------------------------------------------

abhiR::reload()
dropdir <- path(find_dropbox(), 'NIAMS','Ward','USRDS2015','data')

analytic_whites <- read_fst(path(dropdir, 'Analytic_Whites.fst')) %>%
  mutate(REGION = factor(REGION))
analytic_whites_byagegrp <- split(analytic_whites, analytic_whites$AGEGRP)

# Modeling ----------------------------------------------------------------

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
  plt <- ggplot(d, aes(time, -log(estimate)))+geom_point() + 
    geom_abline(color='red') + 
    labs(x = 'Time', y = "Cumulative Hazard")

}

plot_disc_weib <- map(cs_weibull, plot_cs)
plot_disc_ln <- map(cs_lognormal, plot_cs)
plot_disc_ll <- map(cs_loglogistic, plot_cs)

#' Comparing all the models, we have the following models for particular
#' age groups:
#'
#' Weibull: 1,2,6
#' Lognormal: 3, 4
#' Loglogistic: 5

final_models_disc <- list(
  whites_models_weibull[[1]],
  whites_models_weibull[[2]],
  whites_models_lognormal[[3]],
  whites_models_lognormal[[4]],
  whites_models_loglogistic[[5]],
  whites_models_weibull[[6]]
)
names(final_models_disc) <- names(whites_models_weibull)

##%######################################################%##
#                                                          #
####                 Transplant Models                  ####
#                                                          #
##%######################################################%##


# Exploration -------------------------------------------------------------

plot(survfit(Surv(surv_time, cens_type==3)~AGEGRP, data = analytic_whites))
## This appears to show an exponential or weibull-type model will work better

model_template_transplants <- function(dat, dist = 'weibull'){
  require(rms)
  m1 <- survreg(Surv(surv_time + 0.01, cens_type==2) ~
                  factor(REGION) + rcs(zscore) +
                  SEX*(ESRD_Cause +  BMI2) +
                  Cancer + Cardia + Cva + Hyper + Ihd + Pulmon + Pvasc + Smoke +
                  DIABETES + ALCOH + DRUG + BMI2,
                data = dat, dist=dist)
  return(m1)
}

model_template_transplants_rms <- function(dat, dist = 'weibull'){
  library(rms)
  m1 <- psm(Surv(surv_time + 0.01, cens_type==2) ~
              REGION + SEX*rcs(zscore) +
              SEX*(ESRD_Cause +  BMI2) +
              Cancer + Cardia + Cva + Hyper + Ihd + Pulmon + Pvasc + Smoke +
              DIABETES + ALCOH + DRUG + BMI2,
            data = dat, dist=dist, y = T, x = T)
  return(m1)
}


# Trying transplant models ------------------------------------------------

models1_weib <- map(analytic_whites_byagegrp, model_template_transplants_rms)
models1_ll <- map(analytic_whites_byagegrp, model_template_transplants_rms, dist='loglogistic')
models1_ln <- map(analytic_whites_byagegrp, model_template_transplants_rms, dist='lognormal')

## log-normal wins here
cs_weib <- map(models1_weib, CoxSnell.psm)
cs_ll <- map(models1_ll, CoxSnell.psm)
cs_ln <- map(models1_ln, CoxSnell.psm)

plots_tr_weib <- map(cs_weib, plot_cs)
plots_tr_ll <- map(cs_ll, plot_cs)
plots_tr_ln <- map(cs_ln, plot_cs)

#' Comparing all the models, we have the following models for particular
#' age groups:
#'
#' Weibull:
#' Lognormal: 1, 2, 3, 4, 5, 6
#' Loglogistic:

final_models_tr <- models1_ln

save(final_models_disc, final_models_tr,
     file = path(dropdir, 'whites_models_final.rda'),
     compress = T)

##%######################################################%##
#                                                          #
####                     Model fit                      ####
#                                                          #
##%######################################################%##

cs_disc <- map(final_models_disc, CoxSnell.psm)
plts_disc <- map(cs_disc, plot_cs)
pdf('graphs/Revision/CoxSnellDiscontinuation.pdf')
cowplot::plot_grid(plotlist = plts_disc, nrow = 3, labels = names(plts_disc), 
                   label_size = 9) 
dev.off()

cs_tr <- map(final_models_tr, CoxSnell.psm)
plts_tr <- map(cs_tr, plot_cs)
pdf('graphs/Revision/CoxSnellTransplant.pdf')
cowplot::plot_grid(plotlist = plts_tr, nrow = 3, labels = names(plts_tr),
                   label_size = 9)
dev.off()
