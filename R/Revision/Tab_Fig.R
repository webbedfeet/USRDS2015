##%######################################################%##
#                                                          #
####          Tables and Figures for Revision           ####
#                                                          #
##%######################################################%##



# Setup -------------------------------------------------------------------

abhiR::reload()
dropdir <- here::here('data')
# dropdir <- path("P:/Ward/USRDS2015/data")
# dropdir <- path(find_dropbox(), 'NIAMS','Ward','USRDS2015','data')

analytic <- read_fst(path(dropdir, 'Revision','AnalyticUpdated.fst'))

analytic_filt <- analytic %>%
  mutate(RACE2 = factor(RACE2)) %>%
  mutate(RACE2 = fct_relevel(RACE2, 'White','Black','Asian','Native American','Hispanic')) %>%
  filter(RACE2 != 'Other') %>% # Removes 2371 with Other and 11131 with missing RACE2 (N=1,277,499)
  mutate(RACE2 = fct_drop(RACE2, 'Other'))


analytic_filt <- analytic_filt %>%
  mutate(SEX = ifelse(SEX=='1', 'Male','Female')) %>%
  mutate_at(vars(DIABETES, Cancer,Cardia, Cva, Ihd, Pulmon, Pvasc), as.factor) %>%
  set_variable_labels(DIABETES = 'Diabetes',
                      SEX = 'Gender',
                      INC_AGE = 'Age',
                      zscore = 'Socio-economic score',
                      REGION = 'Region',
                      ESRD_Cause = 'Cause of ESRD',
                      comorb_indx = 'Comorbidity index',
                      ALCOH = "Alcohol use",
                      DRUG = "Drug use",
                      BMI2 = 'Body Mass Index',
                      Smoke = 'Smoking status') %>%
  set_value_labels(ESRD_Cause = c('Glomeruloneph.' = 'Glomerulonephritis'))


# Table One ---------------------------------------------------------------

rmarkdown::render('Tab_Fig.Rmd', output_format=word_document(), output_file = 'TableOne.docx')


# Figures -----------------------------------------------------------------
## We are only doing figures for the multivariate analyses

# results <- readRDS(path(dropdir, 'Revision', 'simResults.rds'))
#
# base <- analytic_filt %>%
#   nest(-AGEGRP) %>%
#   mutate(mods = map(data, ~coxph(Surv(surv_time, cens_type %in% c(1,3))~RACE2, data=.))) %>%
#   mutate(results = map(mods, tidy)) %>%
#   select(AGEGRP, results) %>%
#   unnest() %>%
#   select(AGEGRP, term, estimate) %>%
#   mutate(term = str_remove(term, 'RACE2'),
#          estimate = exp(estimate))


# Simulation results ------------------------------------------------------

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
  filter(str_detect(term, 'RACE')) %>% # Keep only the Race coefficients
  select(AGEGRP, term, estimate) %>% # Keep the point estimates
  mutate(term = str_remove(term, 'RACE2'),
         estimate = exp(estimate)) %>% # Convert to HR
  mutate(term = factor(term), # Re-order levels in desired order
         term = fct_relevel(term, 'Black','Hispanic','Asian','Native American'))

results2 <- readRDS(path(dropdir, 'Revision','SimResultsMult.rds')) # Simulation results

plt2 <- map(names(results2), function(n1){ # List of plots by age groups
  d <- results2[[n1]] %>%
    mutate(term = as.factor(term)) %>%
    mutate(term = fct_relevel(term, 'Black','Hispanic','Asian','Native American')) # Re-order levels in desired order

  ggplot(d, aes(x = estimate)) +
    # geom_histogram(bins=20) +
    geom_histogram(bins = 50)+ # Desire histogram rather than density
    geom_vline(data = dplyr::filter(base2, AGEGRP==n1),
               aes(xintercept = estimate),
               color = 'red') +
    facet_wrap(~term, scales = 'free_y') +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank())+
    labs(x = 'Hazard ratio against whites',
         y = '')
}
)

## Re-format the age groups into hyphenated expressions
bl <- names(results) %>% str_match_all('\\d+') %>%
  map(~as.numeric(t(.))) %>%
  do.call(rbind, .) %>%
  as_tibble() %>%
  mutate(V1 = ifelse(V1 %% 10 == 9, V1+1, V1)) %>%
  unite(labs, c("V1",'V2'), sep=' - ')


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
  geom_vline(data = nominal_model_overall,
             aes(xintercept = HR),
             color='red',size = 1.5)+
  # geom_segment(data = nominal_model_overall,
  #              aes(x = HR, xend=HR, yend = 0, y = 0.03),
  #              color='red', size = 1.5, arrow = arrow(length = unit(.2, 'cm')))+
  facet_wrap(~term, scales='free_y') +
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

