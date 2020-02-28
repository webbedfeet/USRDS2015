#' This is a template script for the simulation study. The only things that change in the simulation
#' study are the age groups and the particular simulation iteration.
#'
#' We will be performing 1000 simulations for each of the 6 age groups for a total of 6000 iterations.
#' This template will be the basis for 6000 script files generated using `simulation_generate.R` based
#' on this template.
#'
#' For this simulation, `i` (ranging from 1 to 6) denotes the age group and `j` (ranging from
#' 1 to `nsim`) denotes the simulation number. The whites data files are
#' `data/white1.fst` through `data/white6.fst` and the rest data files are
#' `data/rest1.fst` through `data/rest6.fst`. We will need to ensure that the
#' `fst` package is loaded.
#'
#' We will also copy the `whites_models_final.rda` file into `data`.

library(fs)
library(glue)
library(fst)
library(tidyverse)
library(survival)
library(rms)
library(here)

set.seed(57515)
i <- 1
j <- 100

data_path <- 'P:/Ward/USRDS2015/data/Revision/biowulf/'
output_path <- here('R/Revision/local/output')
# load(as.character(glue('/data/dasgupab/USRDS2015/data/biowulf/white_models_final{i}.rda')))
load(file.path(data_path,as.character(glue('white_models_final{i}.rda')))) # white models for discontinuation and transplant

white_data <- fst::read_fst(file.path(data_path, as.character(glue('whites{i}.fst'))))
rest_data <- fst::read_fst(file.path(data_path, as.character(glue('rest{i}.fst'))))

# Generate linear predictors ----

library(rms)
lp <- tibble(disc = predict(white_model_disc, newdata = rest_data, type='lp'),
              tr = predict(white_model_tr, newdata = rest_data, type='lp'))
scls <- tibble(disc = white_model_disc$scale,
               tr = white_model_tr$scale)

# Inverse CDF for distributions ----

invcdf <- function(u, x = 'weibull'){{
    case_when(x == 'weibull' ~ log(-log(u)),
              x == 'lognormal' ~ qnorm(u),
              x == 'loglogistic' ~ log(u/(1-u)))
}}

# Multivariate simulations ----

whites_reduced <- white_data %>%
    select(USRDS_ID, toc:tow, surv_time, cens_type, RACE2,
           REGION, SEX, zscore, ESRD_Cause, BMI2, comorb_indx,
           DIABETES, ALCOH, DRUG) %>%
    mutate(disc = NA, tr = NA,
           new_surv_time = surv_time,
           new_cens_type = cens_type)

N <- nrow(rest_data)

unifs <- matrix(runif(N*2), ncol=2)
es <- tibble(disc = invcdf(unifs[,1], white_model_disc$dist),
             tr = invcdf(unifs[,2], white_model_tr$dist))
scl <- tibble(disc = rep(scls$disc, nrow(lp)),
              tr = rep(scls$tr, nrow(lp)))
R <- exp(lp + scl * es)
dat  <- select(rest_data, USRDS_ID, toc:tow, surv_time, cens_type, RACE2,
           REGION, SEX, zscore, ESRD_Cause, BMI2, comorb_indx,
           DIABETES, ALCOH, DRUG)
dat <- cbind(dat, R) %>%
    mutate(new_surv_time = pmin(toc, tod, disc, tr, na.rm=T)) %>%
    mutate(new_cens_type =
                case_when(
                          toc == new_surv_time ~ 0,
                          tod == new_surv_time ~ 1,
                          tr == new_surv_time ~ 2,
                          disc == new_surv_time ~ 3
                          )) %>%
    mutate(new_surv_time = ifelse(new_cens_type==3,
                                  new_surv_time + 7/365.25,
                                  new_surv_time)) %>%
    bind_rows(whites_reduced)
#
# mod1 <- coxph(Surv(new_surv_time, new_cens_type %in% c(1,3))~
#               RACE2 + REGION + SEX + rcs(zscore) +
#               ESRD_Cause +
#               rcs(comorb_indx) +
#               # Cancer + Cardia + Cva + Hyper + Ihd + Pulmon + Pvasc + Smoke +
#               DIABETES + ALCOH + DRUG + BMI2 +
#               SEX:DIABETES + SEX:ALCOH + SEX:DRUG +
#               SEX:BMI2 + SEX:ESRD_Cause + SEX:REGION +
#               SEX*rcs(comorb_indx),
#               data = dat, y=F)
#
# saveRDS(mod1, file.path(output_path, as.character(glue('model{i}_{j}.rds'))), compress=T)
# write_fst(dat, path(output_path, as.character(glue('simdata{i}_{j}.fst'))))

