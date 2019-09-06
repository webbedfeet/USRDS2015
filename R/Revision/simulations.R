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

invcdf <- function(u, x){
  case_when(x == 'weibull' ~ log(-log(u)),
            x == 'lognormal' ~ qnorm(u),
            x == 'loglogistic' ~ log(u/(1-u)))
}


# Simulate uniforms -------------------------------------------------------

nsim <- 100
set.seed(10283)

ns <- map_int(analytic_rest_byagegrp, nrow)
Unifs <- map(ns, ~tibble(disc = runif(nsim*.), tr = runif(nsim*.)))
Es <- pmap(Unifs, final_models_disc, final_models_tr, 
           function(x, y, z) {
             x %>% 
               mutate(disc = invcdf(disc, y$dist),
                      tr = invcdf(tr, z$dist))
           })
