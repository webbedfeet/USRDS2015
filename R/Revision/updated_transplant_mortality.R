# Modeling for simulations

#' We will add a mortality model that encompasses all the races, which will
#' provide predicted death times for individuals who have missing tod. The
#' counterfactual nature of this model mainly refers to what could have happened
#' if transplanted individuals did not receive their transplant. This mortality
#' experience will reflect what is observed in each agegroup and each race, and
#' will not involve counterfactual arguments across races, unlike what we do for
#' the discontinuation and transplant processes. We will restrict this model to
#' individuals who either were on the transplant list
#' (`!is.na(CAN_FIRST_LISTING_DT)` ) or received a transplant
#' (`!is.na(TX1DATE)`).
#'
#' This is basically a case of doing a single imputation of a death date for
#' each individual who was transplanted, that reflects the estimated death date
#' if the individual didn't get a transplant. In the event that the estimated
#' death date is before the observed transplant date, we will keep the death
#' date missing. This strategy does not change the observed survival times or event types
#'
#' We will fit parametric survival models within each age group using the
#' function `rms::psm`, and evaluate the adequacy of model fit using Harrell's
#' c-index and Cox-Snell residuals. Once the models are chosen, we will estimate
#' the expected survival time for each individual using `rms::Mean`, which gives
#' the function for the mean survival from the fitted model.
#'
#' We will then update the analytic dataset with these new estimated times of
#' death for use in the simulation models in `simuation.R`. The new analytic
#' dataset will be stored in `data/Revision/AnalyticUpdated2.fst`.
#'
#'
#+ preamble, include = F
suppressPackageStartupMessages(library(tidyverse))
library(RSQLite)
library(dbplyr)
library(fs)
library(fst)
knitr::opts_chunk$set(warning=F, message=F)

#' ### Create datasets
#'
#+ data, echo = F
sql_conn <- dbConnect(SQLite(),
                      'data/raw/USRDS.sqlite3')
studyid_db <- tbl(sql_conn, 'StudyIDs')
patient <- tbl(sql_conn, 'patients')
on_transplant_list <- studyid_db %>%
  left_join(patient) %>%
  select(USRDS_ID, CAN_FIRST_LISTING_DT) %>%
  collect(n=Inf)
analytic_data <- read_fst(path('data','Revision','AnalyticUpdated.fst'))
analytic_data <- analytic_data %>% left_join(on_transplant_list) %>%
  mutate(on_tr_list = !is.na(CAN_FIRST_LISTING_DT)) %>%
  mutate(transplant_eligible = on_tr_list | cens_type==2) # On list or got transplant

analytic_data_tr <- analytic_data %>% filter(transplant_eligible) %>%
  filter(RACE2!='Other') %>%
  mutate(RACE2 = recode_factor(RACE2,'Native American'='AI/AN'),
         RACE2 = fct_relevel(RACE2, 'White', "Black", 'Asian','Hispanic','AI/AN'))
analytic_data_tr_age <- split(analytic_data_tr, analytic_data_tr$AGEGRP)


# Modeling ----------------------------------------------------------------

library(furrr) # for parallel computation using map
library(purrr)

plan(multiprocess) # Start core clusters

model_template_rms <- function(dat, dist = 'weibull'){
  require(rms)
  m1 <- psm(Surv(surv_time + 0.01, cens_type==2) ~
              RACE2 + REGION + SEX + rcs(zscore) +
              ESRD_Cause +
              rcs(comorb_indx) +
              # Cancer + Cardia + Cva + Hyper + Ihd + Pulmon + Pvasc + Smoke +
              DIABETES +
              #ALCOH + DRUG + # Removed because lots of missing, doesn't affect prediction
              BMI2 +
              SEX:DIABETES +
              #SEX:ALCOH + SEX:DRUG +
              SEX:BMI2 + SEX:ESRD_Cause + SEX:REGION +
              SEX*rcs(comorb_indx) +
              SEX*rcs(zscore)+
              RACE2*(SEX+DIABETES+
                       # ALCOH+DRUG+
                       BMI2+ESRD_Cause+REGION) +
              RACE2*rcs(comorb_indx)+
              RACE2*rcs(zscore),
            data = dat, dist=dist, y = T, x = T)
  return(m1)
}

cindex.cens <- function(mod, dat, type=c('time','hazard')){
  require(rms)
  dxy = dxy.cens(predict(mod), Surv(dat$surv_time+0.01, dat$cens_type==2), type=type)
  out = 0.5*(dxy['Dxy'] + 1)
  return(out)
}

weibull_models <- future_map(analytic_data_tr_age, model_template_rms,.progress=TRUE)
lognormal_models <- future_map(analytic_data_tr_age, model_template_rms, dist='lognormal', .progress=T)
loglogistic_models <- future_map(analytic_data_tr_age, model_template_rms,
                                 dist='loglogistic',
                                 .progress=TRUE)


# Model assessment and validation -----------------------------------------

## C-index
weibull_cindex <- map2_dbl(weibull_models, analytic_data_tr_age, cindex.cens)
lognormal_cindex <- map2_dbl(lognormal_models, analytic_data_tr_age, cindex.cens)
loglogistic_cindex <- map2_dbl(loglogistic_models, analytic_data_tr_age, cindex.cens)
rbind(weibull_cindex, lognormal_cindex, loglogistic_cindex)

## Cox-Snell residuals
cs_weibull <- map(weibull_models, CoxSnell)
cs_loglogistic <- map(loglogistic_models, CoxSnell)
cs_lognormal <- map(lognormal_models, CoxSnell)

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

pdf('graphs/Revision/CSExplore_transplant_mortality.pdf')
for(i in 1:6){
  print(cowplot::plot_grid(plot_disc_weib[[i]], plot_disc_ln[[i]],plot_disc_ll[[i]], nrow=2))
}
dev.off()

#' We will use a log-normal model for all of the age strata, based on both c-index and
#' Cox-Snell residuals


# Prediction and imputation -----------------------------------------------

tod_new <- map2(lognormal_models, analytic_data_tr_age, ~Mean(.x)(predict(.x, .y, type='lp')))

map2(tod_new, analytic_data_tr_age, ~mean(.x[.y$cens_type==2]<.y$tot[.y$cens_type==2], na.rm=T))

bl=map2(analytic_data_tr_age, tod_new, ~cbind(.x, 'tod_new'=.y)) %>% bind_rows()
bl <- bl %>%
  filter(cens_type==2) %>%
  mutate(tod = ifelse(tot <= tod_new, tod_new, NA))

test_analytic <- analytic_data
ind <- match(bl$USRDS_ID, test_analytic$USRDS_ID)
test_analytic$tod[ind] <- bl$tod

fst::write_fst(test_analytic, 'data/Revision/AnalyticUpdated2.fst')
