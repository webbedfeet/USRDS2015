## This is code for investigating the nature of the simulations and validating
## whether the simulations are reasonable or not.

##%######################################################%##
#                                                          #
####             Counterfactual simulations             ####
#                                                          #
##%######################################################%##

## This code develops counterfactual simulated survival times in non-whites that
## represent white behavior in those groups


# Setup -------------------------------------------------------------------

abhiR::reload()
# dropdir <- path(find_dropbox(), 'NIAMS','Ward','USRDS2015','data')
dropdir <- 'P:/Ward/USRDS2015/data'
if(!dir_exists(dropdir)) dropdir <- 'data'

analytic_whites <- read_fst(path(dropdir,'Revision', 'Analytic_Whites.fst')) %>%
  mutate(REGION = factor(REGION))
analytic_whites_byagegrp <- split(analytic_whites, analytic_whites$AGEGRP)
analytic_rest <- read_fst(path(dropdir, 'Revision','Analytic_Rest.fst')) %>%
  mutate(REGION = factor(REGION))
analytic_rest_byagegrp <- split(analytic_rest, analytic_rest$AGEGRP)
analytic <- read_fst(path(dropdir, 'Revision', "AnalyticUpdated.fst"))

analytic_filt <- analytic %>%
  mutate(RACE2 = factor(RACE2)) %>%
  mutate(RACE2 = fct_relevel(RACE2, 'White')) %>%
  filter(RACE2 != 'Other') %>%
  mutate(RACE2 = fct_drop(RACE2, 'Other'))

load(path(dropdir, 'Revision','whites_models_final.rda'))

library(foreach)
library(parallel)
library(doParallel)
no_cores <- detectCores()-1




# Generate linear predictors ----------------------------------------------

library(rms)
lps <- pmap(list(analytic_rest_byagegrp, final_models_disc, final_models_tr),
            function(x,y,z) tibble(disc = predict(y, newdata=x, type='lp'),
                    tr = predict(z, newdata=x, type = 'lp')))
scls <- map2(final_models_disc, final_models_tr,
             ~tibble(disc = .x$scale, tr = .y$scale))


# Inverse CDF for distributions -------------------------------------------

invcdf <- function(u, x = 'weibull'){
  case_when(x == 'weibull' ~ log(-log(u)),
            x == 'lognormal' ~ qnorm(u),
            x == 'loglogistic' ~ log(u/(1-u)))
}

results <- vector('list', 6)
nsim <- 10
set.seed(10283)
cl <- makeCluster(no_cores)
registerDoParallel(cl)

for(i in 1:6){
  print(i)
  N <- nrow(analytic_rest_byagegrp[[i]])
  results[[i]] <- foreach(j = 1:nsim, .packages = c('tidyverse', 'broom', 'survival','rms'), .combine=rbind) %dopar% {
    # print(j)
    unifs <- matrix(runif(N*2), ncol = 2)
    es <- tibble(disc = invcdf(unifs[,1], final_models_disc[[i]]$dist),
                 tr = invcdf(unifs[,2], final_models_tr[[i]]$dist))
    lp <- lps[[i]]
    scl <- tibble(disc = rep(scls[[i]]$disc, nrow(lp)),
                  tr = rep(scls[[i]]$tr, nrow(lp)))
    R <- exp(lp + scl * es)
    dat <- select(analytic_rest_byagegrp[[i]],
                  USRDS_ID,toc:tow, surv_time, cens_type, RACE2,
                  REGION, SEX, zscore, ESRD_Cause, BMI2, comorb_indx,
                  DIABETES, ALCOH, DRUG)
    dat <- cbind(dat, R) %>%
      mutate(new_surv_time = pmin(toc, tod, disc, tr, na.rm=T)) %>%
      mutate(  new_cens_type = case_when(
        toc == new_surv_time ~ 0,
        tod == new_surv_time ~ 1,
        tr == new_surv_time ~ 2,
        disc == new_surv_time ~ 3
      )) %>%
      mutate(new_surv_time = ifelse(new_cens_type ==3,
                                    new_surv_time + 7/365.25,
                                    new_surv_time)) %>%
      bind_rows(whites_byage[[i]]) %>%
      mutate(RACE2 = fct_relevel(RACE2, 'White', 'Black','Hispanic','Asian')) %>%
      filter(RACE2 != 'Other') %>%
      mutate(RACE2 = fct_drop(RACE2))

    return(dat %>% select(cens_type, new_cens_type, RACE2))
  }
}

stopCluster(cl)

transition_prob <- function(d){
  out <- d %>% count(cens_type, new_cens_type) %>%
    spread(new_cens_type, n) %>%
    replace_na(list(`0`=0,`1`=0,`2`=0, `3`=0)) %>%
    mutate(rowtot = `0`+`1`+`2`+`3`) %>%
    mutate_at(vars(-rowtot, -cens_type), ~./rowtot) %>%
    select(-rowtot)
  return(out)
}
