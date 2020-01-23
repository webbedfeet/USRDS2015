# Developing parametric survival models for discontinuation and
# transplant outcomes for the white subgroup


# Setup -------------------------------------------------------------------

abhiR::reload()
# dropdir <- path(find_dropbox(), 'NIAMS','Ward','USRDS2015','data')

dropdir <- 'P:/Ward/USRDS2015/data'
if(!dir_exists(dropdir)) dropdir <- 'data'
analytic_whites <- read_fst(path(dropdir, 'Revision', 'Analytic_Whites2.fst'))
analytic_whites_byagegrp <- split(analytic_whites, analytic_whites$AGEGRP)


# Modeling ----------------------------------------------------------------

# model_template <- function(dat, dist = 'weibull'){
#   require(rms)
#   m1 <- survreg(Surv(surv_time + 0.01, cens_type==3) ~
#                   factor(REGION) + SEX*rcs(zscore) +
#                   SEX*(ESRD_Cause +  BMI2) +
#                   comorb_indx +
#                   # Cancer + Cardia + Cva + Hyper + Ihd + Pulmon + Pvasc + Smoke +
#                   DIABETES + ALCOH + DRUG ,
#                 data = dat, dist=dist)
#   return(m1)
# }

model_template_rms <- function(dat, dist = 'weibull'){
  require(rms)
  m1 <- psm(Surv(surv_time + 0.01, cens_type==3) ~
              REGION + SEX + rcs(zscore) +
              ESRD_Cause +
              rcs(comorb_indx) +
              # Cancer + Cardia + Cva + Hyper + Ihd + Pulmon + Pvasc + Smoke +
              DIABETES + ALCOH + DRUG + BMI2 +
              SEX:DIABETES + SEX:ALCOH + SEX:DRUG +
              SEX:BMI2 + SEX:ESRD_Cause + SEX:REGION +
              SEX*rcs(comorb_indx),
                data = dat, dist=dist, y = T, x = T)
  return(m1)
}


whites_models_weibull <- map(analytic_whites_byagegrp, model_template_rms)
whites_models_loglogistic <- map(analytic_whites_byagegrp, model_template_rms, dist = 'loglogistic')
whites_models_lognormal <- map(analytic_whites_byagegrp, model_template_rms, dist = 'lognormal')

# CoxSnell.psm <- function(m, plot = F){
#   require(rms)
#   if(!is(m, 'psm')) stop("Model needs to be run using rms::psm")
#   r <- residuals(m)
#   cs <- -log(attr(r,'theoretical')(exp(r[,1]))) # exp for weibull, loglogistic and lognormal
#   if(plot){
#     survd <- broom::tidy(survfit(Surv(cs, r[,2])~1)) # Compute survival curve of censored CS resid
#     ggplot(survd, aes(time, -log(estimate)))+geom_point() + geom_abline(color = 'red')
#   }
#   return(Surv(cs, r[,2]))
# }

cs_weibull <- map(whites_models_weibull, CoxSnell)
cs_loglogistic <- map(whites_models_loglogistic, CoxSnell)
cs_lognormal <- map(whites_models_lognormal, CoxSnell)

plot_cs <- function(cs){
  require(rms)
  require(broom)
  d <- tidy(npsurv(cs~1))
  plt <- ggplot(d, aes(time, -log(estimate)))+geom_line()+#geom_point(size=1) +
    geom_abline(color='red') +
    labs(x = 'Cox-Snell Residuals', y = "Cumulative Hazard")
  return(plt)
}

plot_disc_weib <- map(cs_weibull, plot_cs)
plot_disc_ln <- map(cs_lognormal, plot_cs)
plot_disc_ll <- map(cs_loglogistic, plot_cs)

pdf('graphs/Revision/CSExplore_discontinuation.pdf')
for(i in 1:6){
  print(cowplot::plot_grid(plot_disc_weib[[i]], plot_disc_ln[[i]],plot_disc_ll[[i]], nrow=2))
}
dev.off()


#' Comparing all the models, we have the following models for particular
#' age groups:
#'
#' Weibull: 1,5,6
#' Lognormal: 3
#' Loglogistic: 2,4

final_models_disc <- list(
  whites_models_weibull[[1]],
  whites_models_loglogistic[[2]],
  whites_models_lognormal[[3]],
  whites_models_loglogistic[[4]],
  whites_models_weibull[[5]],
  whites_models_weibull[[6]]
)
names(final_models_disc) <- names(whites_models_weibull)

##%######################################################%##
#                                                          #
####                 Transplant Models                  ####
#                                                          #
##%######################################################%##


# Exploration -------------------------------------------------------------

plot(survfit(Surv(surv_time, cens_type==2)~AGEGRP, data = analytic_whites))
## This appears to show an exponential or weibull-type model will work better

# model_template_transplants <- function(dat, dist = 'weibull'){
#   require(rms)
#   m1 <- survreg(Surv(surv_time + 0.01, cens_type==2) ~
#                   factor(REGION) + SEX + rcs(zscore) +
#                   ESRD_Cause +
#                   rcs(comorb_indx) +
#                   # Cancer + Cardia + Cva + Hyper + Ihd + Pulmon + Pvasc + Smoke +
#                   DIABETES + ALCOH + DRUG + BMI2,
#                 data = dat, dist=dist)
#   return(m1)
# }

model_template_transplants_rms <- function(dat, dist = 'weibull'){
  library(rms)
  m1 <- psm(Surv(surv_time + 0.01, cens_type==2) ~
              REGION + SEX + rcs(zscore) +
              ESRD_Cause +
              rcs(comorb_indx) +
              # Cancer + Cardia + Cva + Hyper + Ihd + Pulmon + Pvasc + Smoke +
              DIABETES + ALCOH + DRUG + BMI2 +
              SEX:DIABETES + SEX:ALCOH + SEX:DRUG +
              SEX:BMI2 + SEX:ESRD_Cause + SEX:REGION +
              SEX*rcs(comorb_indx),
            data = dat, dist=dist, y = T, x = T)
  return(m1)
}


# Trying transplant models ------------------------------------------------

models1_weib <- map(analytic_whites_byagegrp, model_template_transplants_rms)
models1_ll <- map(analytic_whites_byagegrp, model_template_transplants_rms, dist='loglogistic')
models1_ln <- map(analytic_whites_byagegrp, model_template_transplants_rms, dist='lognormal')

## log-normal wins here
cs_weib <- map(models1_weib, CoxSnell)
cs_ll <- map(models1_ll, CoxSnell)
cs_ln <- map(models1_ln, CoxSnell)

plots_tr_weib <- map(cs_weib, plot_cs)
plots_tr_ll <- map(cs_ll, plot_cs)
plots_tr_ln <- map(cs_ln, plot_cs)

pdf('graphs/Revision/CSExplore_transplant.pdf')
for(i in 1:6){
  print(cowplot::plot_grid(plots_tr_weib[[i]], plots_tr_ln[[i]],plots_tr_ll[[i]], nrow=2))
}
dev.off()

#' Comparing all the models, we have the following models for particular
#' age groups:
#'
#' Weibull:
#' Lognormal: 1, 2, 3, 4, 5, 6
#' Loglogistic:

final_models_tr <- models1_ln

save(final_models_disc, final_models_tr,
     file = path(dropdir,'Revision', 'whites_models_final.rda'),
     compress = T)

#
##%######################################################%##
#                                                          #
####                     Model fit                      ####
#                                                          #
##%######################################################%##

cs_disc <- map(final_models_disc, CoxSnell)
plts_disc <- map(cs_disc, plot_cs)
pdf('graphs/Revision/CoxSnellDiscontinuation.pdf')
cowplot::plot_grid(plotlist = plts_disc, nrow = 3, labels = names(plts_disc),
                   label_size = 9)
dev.off()

cs_tr <- map(final_models_tr, CoxSnell)
plts_tr <- map(cs_tr, plot_cs)
pdf('graphs/Revision/CoxSnellTransplant.pdf')
cowplot::plot_grid(plotlist = plts_tr, nrow = 3, labels = names(plts_tr),
                   label_size = 9)
dev.off()


# Cox-Snell final models --------------------------------------------------

load(path(dropdir,'Revision', 'whites_models_final.rda'))
cs_disc <- map(final_models_disc, CoxSnell)
cs_tr <- map(final_models_tr, CoxSnell)
plots_disc <- map(cs_disc, plot_cs)
plots_tr <- map(cs_tr, plot_cs)

pdf('graphs/Revision/CoxSnellDiscontinuation.pdf')
for(i in 1:length(plots_disc)){
  print(plots_disc[[i]])
}
dev.off()
pdf('graphs/Revision/CoxSnellTransplant.pdf')
for(i in 1:length(plots_tr)){
  print(plots_tr[[i]])
}
dev.off()
