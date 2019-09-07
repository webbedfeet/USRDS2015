##%######################################################%##
#                                                          #
####             Counterfactual simulations             ####
#                                                          #
##%######################################################%##

## This code develops counterfactual simulated survival times in non-whites that
## represent white behavior in those groups


# Setup -------------------------------------------------------------------

abhiR::reload()
dropdir <- path(find_dropbox(), 'NIAMS','Ward','USRDS2015','data')

analytic_whites <- read_fst(path(dropdir, 'Analytic_Whites.fst')) %>% 
  mutate(REGION = factor(REGION))
analytic_whites_byagegrp <- split(analytic_whites, analytic_whites$AGEGRP)
analytic_rest <- read_fst(path(dropdir, 'Analytic_Rest.fst')) %>% 
  mutate(REGION = factor(REGION))
analytic_rest_byagegrp <- split(analytic_rest, analytic_rest$AGEGRP)

load(path(dropdir, 'whites_models_final.rda'))


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


# Simulate uniforms -------------------------------------------------------

nsim <- 100
set.seed(10283)

whites_byage <- map(analytic_whites_byagegrp, 
                    function(d){ d %>% 
                      select(USRDS_ID,toc:tow, surv_time, cens_type, RACE2) %>% 
                      mutate(disc = NA, tr = NA, 
                             new_surv_time = surv_time, new_cens_type = cens_type) 
                      })


ns <- map_int(analytic_rest_byagegrp, nrow)

i <- 3
N <- nrow(analytic_rest_byagegrp[[i]])

results <- vector('list', nsim)

for(j in 1:nsim){
  print(j)
  unifs <- matrix(runif(N*2), ncol = 2)
  es <- tibble(disc = invcdf(unifs[,1], final_models_disc[[i]]$dist),
               tr = invcdf(unifs[,2], final_models_tr[[i]]$dist))
  lp <- lps[[i]]
  scl <- tibble(disc = rep(scls[[i]]$disc, nrow(lp)),
                tr = rep(scls[[i]]$tr, nrow(lp)))
  R <- exp(lp + scl * es)
  dat <- select(analytic_rest_byagegrp[[i]],
                USRDS_ID,toc:tow, surv_time, cens_type, RACE2)
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
  
  mod <- broom::tidy(
    coxph(Surv(new_surv_time, new_cens_type %in% c(1,3))~ RACE2, data = dat)
  ) %>% 
    mutate(term = str_remove(term, 'RACE2')) %>% 
    select(term, estimate) %>% 
    mutate(estimate = exp(estimate))
  
  results[[j]] <- mod
}

res <- rbindlist(results, idcol='sim')

base <- broom::tidy(
  coxph(Surv(surv_time, cens_type %in% c(1,3))~RACE2, data=dat)
) %>% 
  mutate(term = str_remove(term,'RACE2')) %>% 
  select(term, estimate) %>% 
  mutate(estimate = exp(estimate))

ggplot(res, aes(x = estimate)) + geom_histogram() + 
  geom_vline(data=base, aes(xintercept = estimate), color='red') +
  facet_wrap(~term, scales = 'free')

