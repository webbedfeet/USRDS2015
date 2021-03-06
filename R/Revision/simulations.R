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

analytic_whites <- read_fst(path(dropdir,'Revision', 'Analytic_Whites2.fst'))
analytic_whites_byagegrp <- split(analytic_whites, analytic_whites$AGEGRP)
analytic_rest <- read_fst(path(dropdir, 'Revision','Analytic_Rest2.fst'))
analytic_rest_byagegrp <- split(analytic_rest, analytic_rest$AGEGRP)
analytic <- read_fst(path(dropdir, 'Revision', "AnalyticUpdated2.fst"))

assertthat::are_equal(nrow(analytic_whites)+nrow(analytic_rest), nrow(analytic))

# This section was taken care of in updated_transplant_mortality.R
# analytic_filt <- analytic %>%
#   mutate(RACE2 = factor(RACE2)) %>%
#   mutate(RACE2 = fct_relevel(RACE2, 'White')) %>%
#   filter(RACE2 != 'Other') %>%
#   mutate(RACE2 = fct_drop(RACE2, 'Other'))
#
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


# Simulate -------------------------------------------------------

# Univariate simulation results -------------------------------------------


whites_byage <- map(analytic_whites_byagegrp,
                    function(d){ d %>%
                      select(USRDS_ID,toc:tow, surv_time, cens_type, RACE2) %>%
                      mutate(disc = NA, tr = NA,
                             new_surv_time = surv_time, new_cens_type = cens_type)
                      })

results <- vector('list', 6)
nsim <- 1000
set.seed(10283)
cl <- makeCluster(no_cores)
registerDoParallel(cl)

for(i in 1:6){
  print(i)
  N <- nrow(analytic_rest_byagegrp[[i]])
  results[[i]] <- foreach(j = 1:nsim, .packages = c('tidyverse', 'broom', 'survival'), .combine=rbind) %dopar% {
    # print(j)
    unifs <- matrix(runif(N*2), ncol = 2)
    es <- tibble(disc = invcdf(unifs[,1], final_models_disc[[i]]$dist),
                 tr = invcdf(unifs[,2], final_models_tr[[i]]$dist))
    lp <- lps[[i]]
    scl <- tibble(disc = rep(scls[[i]]$disc, nrow(lp)),
                  tr = rep(scls[[i]]$tr, nrow(lp)))
    R <- exp(lp + scl * es)
    dat <- select(analytic_rest_byagegrp[[i]],
                  USRDS_ID,toc:tow, surv_time, cens_type, RACE2)
    dat <- cbind(dat, R) %>% # append white counterfactual times to non-white data
      mutate(new_surv_time = pmin(toc, tod, disc, tr, na.rm=T)) %>%
      mutate(  new_cens_type = case_when(
        toc == new_surv_time ~ 0,
        tod == new_surv_time ~ 1,
        tr == new_surv_time ~ 2,
        disc == new_surv_time ~ 3
      )) %>%
      mutate(new_surv_time = ifelse(new_cens_type ==3,
                                    new_surv_time + 7/365.25, # death is 7 days after discontinuation
                                    new_surv_time)) %>%
      bind_rows(whites_byage[[i]]) %>% # Add on the white data
      mutate(RACE2 = fct_relevel(RACE2, 'White', 'Black','Hispanic','Asian')) %>%
      filter(RACE2 != 'Other') %>%
      mutate(RACE2 = fct_drop(RACE2))

    mod1 <- broom::tidy(
      coxph(Surv(new_surv_time, new_cens_type %in% c(1,3))~ RACE2, data = dat)
    ) %>%
      mutate(term = str_remove(term, 'RACE2')) %>%
      select(term, estimate) %>%
      mutate(estimate = exp(estimate))


    return(mod1)
    # results[[j]] <- mod
  }
}
names(results) <- names(analytic_rest_byagegrp)
saveRDS(results, file = path(dropdir,'Revision', 'simResults.rds'), compress = T)
stopCluster(cl)


# Multivariate simulation results -----------------------------------------


# REGION + SEX*rcs(zscore) +
#   SEX*(ESRD_Cause +  BMI2) +
#   comorb_indx +
#   # Cancer + Cardia + Cva + Hyper + Ihd + Pulmon + Pvasc + Smoke +
#   DIABETES + ALCOH + DRUG + BMI2,

whites_byage <- map(analytic_whites_byagegrp,
                    function(d){ d %>%
                      select(USRDS_ID,toc:tow, surv_time, cens_type, RACE2,
                             REGION, SEX, zscore, ESRD_Cause, BMI2, comorb_indx,
                             DIABETES, ALCOH, DRUG) %>%
                      mutate(disc = NA, tr = NA,
                             new_surv_time = surv_time, new_cens_type = cens_type)
                      })

results <- vector('list', 6)
nsim <- 1000
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

    mod1 <- broom::tidy(
      coxph(Surv(new_surv_time, new_cens_type %in% c(1,3))~
              RACE2 + REGION + SEX + rcs(zscore) +
              ESRD_Cause +
              rcs(comorb_indx) +
              # Cancer + Cardia + Cva + Hyper + Ihd + Pulmon + Pvasc + Smoke +
              DIABETES + ALCOH + DRUG + BMI2 +
              SEX:DIABETES + SEX:ALCOH + SEX:DRUG +
              SEX:BMI2 + SEX:ESRD_Cause + SEX:REGION +
              SEX*rcs(comorb_indx),
              data = dat)
    ) %>%
      filter(str_detect(term, 'RACE2')) %>%
      mutate(term = str_remove(term, 'RACE2')) %>%
      select(term, estimate) %>%
      mutate(estimate = exp(estimate))


    return(mod1)
    # results[[j]] <- mod
  }
}

names(results) <- names(analytic_rest_byagegrp)
# saveRDS(results,
#         file = here::here('data','rda','simResultsMult.rds'),
#         compress=T)
saveRDS(results,
        file = path(dropdir,'Revision', 'simResultsMult.rds'),
        compress = T)
stopCluster(cl)

##%######################################################%##
#                                                          #
####                       Plots                        ####
#                                                          #
##%######################################################%##

results <- readRDS(path(dropdir, 'Revision', 'simResults.rds'))

base <- analytic_filt %>%
  nest(-AGEGRP) %>%
  mutate(mods = map(data, ~coxph(Surv(surv_time, cens_type %in% c(1,3))~RACE2, data=.))) %>%
  mutate(results = map(mods, tidy)) %>%
  select(AGEGRP, results) %>%
  unnest() %>%
  select(AGEGRP, term, estimate) %>%
  mutate(term = str_remove(term, 'RACE2'),
         estimate = exp(estimate))

base2 <- analytic_filt %>%
  nest(-AGEGRP) %>%
  mutate(mods = map(data, ~coxph(Surv(surv_time, cens_type %in% c(1,3))~
                                   RACE2 + REGION + SEX + rcs(zscore) +
                                   ESRD_Cause +
                                   rcs(comorb_indx) +
                                   # Cancer + Cardia + Cva + Hyper + Ihd + Pulmon + Pvasc + Smoke +
                                   DIABETES + ALCOH + DRUG + BMI2 +
                                   SEX:DIABETES + SEX:ALCOH + SEX:DRUG +
                                   SEX:BMI2 + SEX:ESRD_Cause + SEX:REGION +
                                   SEX*rcs(comorb_indx),
                               data=.))) %>%
  mutate(results = map(mods, tidy)) %>%
  select(AGEGRP, results) %>%
  unnest() %>%
  filter(str_detect(term, 'RACE')) %>%
  select(AGEGRP, term, estimate) %>%
  mutate(term = str_remove(term, 'RACE2'),
         estimate = exp(estimate))

plt <- map(names(results),
           ~ggplot(results[[.]], aes(x = estimate)) +
             # geom_histogram(bins=20) +
             geom_density()+
             geom_vline(data = dplyr::filter(base, AGEGRP==.),
                        aes(xintercept = estimate),
                        color = 'red') +
             facet_wrap(~term, scales = 'free') +
             theme_bw() +
             labs(x = 'Hazard ratio against whites',
                  y = '')
           )

results2 <- readRDS(path(dropdir, 'Revision','SimResultsMult.rds'))

plt2 <- map(names(results2),
           ~ggplot(results2[[.]], aes(x = estimate)) +
             # geom_histogram(bins=20) +
             geom_density()+
             geom_vline(data = dplyr::filter(base, AGEGRP==.),
                        aes(xintercept = estimate),
                        color = 'red') +
             facet_wrap(~term, scales = 'free') +
             theme_bw() +
             labs(x = 'Hazard ratio against whites',
                  y = '')
)

bl <- str_remove_all(names(results), '\\[|\\(|\\]|\\)') %>%
  str_split(',') %>%
  do.call(rbind,.) %>%
  as_tibble() %>%
  mutate_all(as.numeric) %>%
  mutate(V1 = ifelse(V1 %% 10 == 9, V1+1, V1)) %>%
  unite('labs',c('V1','V2'), sep = ' - ')


for(i in 1:6) {
  plt[[i]] <- plt[[i]] + ggtitle(paste0('Age ',bl$labs[i]))
  plt2[[i]] <- plt2[[i]] + ggtitle(paste0('Age ', bl$labs[i]))
}

pdf('graphs/Revision/simResults.pdf')
for(i in 1:6) print(plt[[i]])
dev.off()
pdf('graphs/Revision/simResultsMult.pdf')
for(i in 1:6) print(plt2[[i]])
dev.off()



for (nm in names(results)){
  plt <- ggplot(results[[nm]], aes(x = estimate)) + geom_histogram() +
    # geom_vline(data = dplyr::filter(base, AGEGRP==nm),
    #            aes(xintercept = estimate),
    #            color = 'red') +
    facet_wrap(~term, scales='free') +
    theme_bw()
}

##%######################################################%##
#                                                          #
####                  Overall estimates                 ####
#                                                          #
##%######################################################%##

## We can pool the estimates  by using weighted averages of the age-stratum estimates
##

simResults <- readRDS(file = path(dropdir,'Revision', 'simResults.rds'))


N <- analytic %>% count(AGEGRP) %>% mutate(perc = n/sum(n))

hrs <- simResults %>% map(select, estimate) %>% do.call(cbind, .) %>% as.matrix()
overall <- hrs %*% N$perc
overall_results <- tibble(term = simResults[[1]]$term, HR = overall[,1])
overall_results <- overall_results %>%
  mutate(term = fct_relevel(
    factor(term),
    'Black','Hispanic','Asian','Native American'
  ))

nominal_model_overall <-
  coxph(Surv(surv_time, cens_type %in% c(1,3))~RACE2, data=analytic_filt) %>%
  broom::tidy() %>%
  mutate(term = str_remove(term, 'RACE2'),
         HR = exp(estimate)) %>%
  select(term, HR) %>%
  mutate(term = fct_relevel(
    factor(term),
    'Black','Hispanic','Asian','Native American'
  ))


ggplot(overall_results, aes(x = HR, y = ..count../sum(..count..))) + geom_histogram(bins=500) +
  geom_segment(data = nominal_model_overall,
               aes(x = HR, xend=HR, yend = 0, y = 0.03),
               color='red', size = 1.5, arrow = arrow(length = unit(.2, 'cm')))+
  facet_wrap(~term) +
  labs(x = 'Hazard ratio for death or discontinuation
       compared to Whites',
       y = 'Relative frequency',
       title = 'Overall estimates') +
  theme(strip.text = element_text(face='bold'))
ggsave('graphs/Revision/Overall_univariate.pdf')
## Add nominal estimates

nominal_model <- analytic_filt %>%
  group_by(AGEGRP) %>%
  group_modify(~broom::tidy(coxph(Surv(surv_time, cens_type %in% c(1,3))~RACE2, data=.)),
               keep=T) %>%
  mutate(term = str_remove(term, 'RACE2'),
         HR = exp(estimate)) %>%
  select(AGEGRP, term, HR) %>%
  mutate(term = fct_relevel(
    factor(term),
    'Black', 'Hispanic','Asian','Native American'))

theme_set(theme_bw())
names(simResults) <- levels(nominal_model$AGEGRP)

pdf('graphs/Revision/AgeSpecificPlots.pdf')
for (ag in names(simResults)){
d <- simResults[[ag]] %>%
  mutate(term = fct_relevel(factor(term),
                            'Black','Hispanic','Asian','Native American'))

plt <- ggplot() + geom_histogram(data=d, aes(x = estimate, y = ..count../sum(..count..)),
                          bins = 50) +
  geom_segment(data = nominal_model %>% filter(AGEGRP==ag),
               aes(x = HR, xend=HR, yend = 0, y = 0.005),
               color='red', size = 1.5, arrow = arrow(length = unit(.2, 'cm')))+
  facet_wrap(~term, scales='free_x') +
  labs(x = 'Hazard ratio for death or discontinuation
       compared to Whites', y = '',
       title = paste("Age group", ag))+
  theme(
    strip.text = element_text(face='bold')
  )

simResults_stacked <- bind_rows(simResults, .id='AGEGRP')
simResults_stacked <- simResults_stacked %>%
  mutate(AGEGRP = fct_relevel(AGEGRP, '[18,29]')) %>%
  mutate(term = fct_relevel(term, 'Black','Hispanic','Asian'))
nominal_model <- nominal_model %>%
  mutate(estimate = HR)

ggplot(simResults_stacked,
       aes(x = estimate, color = term)) +
  geom_density() +
  geom_vline(xintercept = 1, linetype=2, color = 'red')+
  geom_segment(data = nominal_model, aes(x = estimate, xend=estimate,
                                         y = 5, yend = 0,
                                         color=term),
                size = 1.5, arrow = arrow(length = unit(.2, 'cm')))+
  facet_grid(AGEGRP ~., scales='free_y', switch='y') +
  scale_x_continuous('Hazard ratio', breaks = seq(0.4, 1.5, by=0.1))+
  labs(y = '', color='Race')+
  theme(strip.text = element_text(size = 14, face = 'bold'),
        strip.text.y = element_text(angle = 180), # Rotate the y-axis labels
        strip.background.y = element_rect(fill = 'white'),
        # strip.placement = 'outside', # Move labels outside the borders
        axis.text.y=element_blank(),
        axis.ticks.y = element_blank())
print(plt)
}
dev.off()



# Multivariate models -----------------------------------------------------

simResults2 <- readRDS(path(dropdir, 'Revision','simResultsMult.rds'))

hrs2 <- simResults2 %>% map(select, estimate) %>% do.call(cbind, .) %>% as.matrix()
overall2 <- hrs2 %*% N$perc
overall_results2 <- tibble(term = simResults2[[1]]$term, HR = overall2[,1])
overall_results2 <- overall_results2 %>%
  mutate(term = fct_relevel(
    factor(term),
    'Black','Hispanic','Asian','Native American'
  ))

nominal_model_mult <- analytic_filt %>%
  group_by(AGEGRP) %>%
  group_modify(~broom::tidy(coxph(Surv(surv_time, cens_type %in% c(1,3))~
                                    REGION + SEX + rcs(zscore) +
                                    ESRD_Cause +
                                    rcs(comorb_indx) +
                                    # Cancer + Cardia + Cva + Hyper + Ihd + Pulmon + Pvasc + Smoke +
                                    DIABETES + ALCOH + DRUG + BMI2 +
                                    SEX:DIABETES + SEX:ALCOH + SEX:DRUG +
                                    SEX:BMI2 + SEX:ESRD_Cause + SEX:REGION +
                                    SEX*rcs(comorb_indx), data=.)),
               keep=T) %>%
  filter(str_detect(term, 'RACE2')) %>%
  mutate(term = str_remove(term, 'RACE2'),
         HR = exp(estimate)) %>%
  select(AGEGRP, term, HR) %>%
  mutate(term = fct_relevel(
    factor(term),
    'Black', 'Hispanic','Asian','Native American'))

ggplot(overall_results2, aes(x = HR, y = ..count../sum(..count..))) + geom_histogram(bins=500) +
  geom_segment(data = nominal_model_overall,
               aes(x = HR, xend=HR, yend = 0, y = 0.03),
               color='red', size = 1.5, arrow = arrow(length = unit(.2, 'cm')))+
  facet_wrap(~term) +
  labs(x = 'Hazard ratio for death or discontinuation
       compared to Whites',
       y = 'Relative frequency',
       title = 'Overall estimates') +
  theme(strip.text = element_text(face='bold'))
ggsave('graphs/Revision/Overall_mult.pdf')

names(simResults2) <- levels(nominal_model_mult$AGEGRP)

pdf('graphs/Revision/AgeSpecificPlotsMult.pdf')
for (ag in names(simResults2)){
  d <- simResults2[[ag]] %>%
    mutate(term = fct_relevel(factor(term),
                              'Black','Hispanic','Asian','Native American'))

  plt <- ggplot() + geom_histogram(data=d, aes(x = estimate, y = ..count../sum(..count..)),
                                   bins = 50) +
    geom_segment(data = nominal_model_mult %>% filter(AGEGRP==ag),
                 aes(x = HR, xend=HR, yend = 0, y = 0.005),
                 color='red', size = 1.5, arrow = arrow(length = unit(.2, 'cm')))+
    facet_wrap(~term, scales='free_x') +
    labs(x = 'Hazard ratio for death or discontinuation
       compared to Whites', y = '',
         title = paste("Age group", ag))+
    theme(
      strip.text = element_text(face='bold')
    )

  simResults_stacked2 <- bind_rows(simResults2, .id='AGEGRP')
  simResults_stacked2 <- simResults_stacked2 %>%
    mutate(AGEGRP = fct_relevel(AGEGRP, '[18,29]')) %>%
    mutate(term = fct_relevel(term, 'Black','Hispanic','Asian'))
  nominal_model_mult <- nominal_model_mult %>%
    mutate(estimate = HR)

  ggplot(simResults_stacked2,
         aes(x = estimate, color = term)) +
    geom_density() +
    geom_vline(xintercept = 1, linetype=2, color = 'red')+
    geom_segment(data = nominal_model_mult, aes(x = estimate, xend=estimate,
                                           y = 5, yend = 0,
                                           color=term),
                 size = 1.5, arrow = arrow(length = unit(.2, 'cm')))+
    facet_grid(AGEGRP ~., scales='free_y', switch='y') +
    scale_x_continuous('Hazard ratio', breaks = seq(0.4, 1.5, by=0.1))+
    labs(y = '', color='Race')+
    theme(strip.text = element_text(size = 14, face = 'bold'),
          strip.text.y = element_text(angle = 180), # Rotate the y-axis labels
          strip.background.y = element_rect(fill = 'white'),
          # strip.placement = 'outside', # Move labels outside the borders
          axis.text.y=element_blank(),
          axis.ticks.y = element_blank())
  print(plt)
}
dev.off()
