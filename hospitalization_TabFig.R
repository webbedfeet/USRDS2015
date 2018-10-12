##%######################################################%##
#                                                          #
####    Tables and figures for hospitalization study    ####
#                                                          #
##%######################################################%##


# setup ---------------------------------------------------------------------------------------

ProjTemplate::reload()
dbdir = verifyPaths(); dir.exists(dbdir)
dropdir <- file.path(ProjTemplate::find_dropbox(), 'NIAMS','Ward','USRDS2015','data')
load(file.path(dropdir, 'modeling_data.rda'))


# Table 1 -------------------------------------------------------------------------------------

getTable1 <- function(d){
  out <- d %>% 
    mutate(age_cat = case_when(
      INC_AGE <=49 ~ '18-49',
      INC_AGE %in% seq(50,59) ~ '50-59',
      INC_AGE %in% seq(60,69) ~ '60-69',
      INC_AGE %in% seq(70,79) ~ '70-79',
      INC_AGE >= 80 ~ '80+'
    ),
    REGION = factor(REGION, levels = c('Northeast','South','Midwest','West')),
    SEX = factor(ifelse(SEX=='1','Male','Female'),levels = c('Male','Female')),
    time_on_dialysis = as.numeric(time_on_dialysis)) %>% 
    mutate(Race = fct_relevel(Race, 
                              c("White", "Black",'Hispanic','Asian','Native American'))%>% 
                           fct_recode(c("AI/AN" = 'Native American'))) %>% 
    select(Race, Age = age_cat, Sex = SEX, Region = REGION, SES = zscore, 
           `Comorbidity index` = comorb_indx, 
           `Time on dialysis` = time_on_dialysis) %>% 
    tableone::CreateTableOne(data = ., strata = 'Race', 
                             test = F, 
                             vars = setdiff(names(.), c('Race')))
  out <- print(out, 
               nonnormal = c("Comorbidity index", 'SES', "Time on dialysis"))
  out <- as.data.frame(out) %>% rownames_to_column('Variable')
  return(out)
}

tab1 <- map(modeling_data[c('stroke_primary','LuCa','dement','thrive')], getTable1)
names(tab1) <- c('Stroke', 'Lung cancer', 'Dementia', "Failure to thrive")
tab1 <- tab1 %>% bind_rows(.id = 'Event') %>% 
  clean_cols(Event)
openxlsx::write.xlsx(tab1, file = 'TableOne.xlsx', colWidths = 'auto')


# Table 2 -------------------------------------------------------------------------------------

load(file.path(dropdir, 'modeling_data.rda'))
race_order <- c("White",'Black','Hispanic','Asian','AI/AN')
events <- c('stroke_primary' = 'Stroke',
            'LuCa' = "Lung cancer",
            'dement' = "Dementia",
            'thrive' = 'Failure to thrive')

## Discontinuation

### Crude
fit_list = map(modeling_data, ~survfit(Surv(time_from_event, cens_type==3)~ Race,
                                       data = .))
cph1_list <-  map(modeling_data, ~ coxph(Surv(time_from_event, cens_type==3)~ Race,
                                         data = .))
logrank_list <- map(cph1_list, ~format.pval(anova(.)[2,4], eps=1e-6))
for(i in 1:6){
  plt_list[[i]] <- survMisc::autoplot(fit_list[[i]], type='single', censSize = 0,
                                      title = names(fit_list)[i],
                                      xLab = 'Days from index event',
                                      legTitle = '')$plot +
    scale_y_continuous('Percent who discontinued dialysis', labels = scales::percent)+
    annotate('text', x = 50, y = 0.1, label=paste0('p-value : ', logrank_list[[i]]),hjust=0) +
    theme(legend.position = 'bottom', legend.justification = c(0.5, 0.5))
}

out_crude <- map(cph1_list, ~tidy(.) %>% select(term, estimate, starts_with('conf')) %>% 
                   mutate_if(is.numeric, exp) %>% 
                   mutate(term = str_remove(term, 'Race')) %>% 
                   mutate(res = as.character(
                     glue('{round(estimate,2)} ({round(conf.low, 2)}, {round(conf.high,2)})'))) %>% 
                   select(term, res) %>% 
                   mutate(term = ifelse(term == "Native American", 'AI/AN', term)) %>% 
                   rbind(data.frame(term="White", res = "1.00")) %>% 
                   rename(cHR_disc = res))

### Adjusted
hosp_coxph <- map(modeling_data,
                  ~ coxph(Surv(time_from_event+0.1, cens_type==3)~Race + agegrp_at_event + SEX + zscore +
                            REGION + comorb_indx + time_on_dialysis, data = .) %>%
                    broom::tidy() %>%
                    filter(str_detect(term, 'Race')) %>%
                    select(term, estimate, p.value:conf.high) %>%
                    mutate(term = str_remove(term, 'Race')) %>%
                    mutate_at(vars(estimate, conf.low:conf.high), exp) %>% 
                    mutate(term = ifelse(term == 'Native American', 'AI/AN', term)) %>% 
                    mutate(res = as.character(glue("{round(estimate,2)} ({round(conf.low, 2)}, {round(conf.high,2)})"))) %>% 
                    select(term, res) %>% 
                    rbind(data.frame(term = "White", res = "1.00")) %>% 
                    rename(aHR_disc = res))

res_discontinutation <- map2(out_crude, hosp_coxph, left_join)
res_discontinutation <-  res_discontinutation[c('stroke_primary','LuCa','dement','thrive')]

## Survival

fit_list_surv = map(modeling_data, ~survfit(Surv(time_from_event, cens_type==1)~ Race,
                                       data = .))
cph1_list_surv <-  map(modeling_data, ~ coxph(Surv(time_from_event, cens_type==1)~ Race,
                                         data = .))
out_crude_surv <- map(cph1_list_surv, 
                      ~tidy(.) %>% select(term, estimate, starts_with('conf')) %>% 
                        mutate_if(is.numeric, exp) %>% 
                        mutate(term = str_remove(term, 'Race')) %>% 
                        mutate(res = as.character(
                          glue('{round(estimate,2)} ({round(conf.low, 2)}, {round(conf.high,2)})'))) %>% 
                        select(term, res) %>% 
                        mutate(term = ifelse(term == "Native American", 'AI/AN', term)) %>% 
                        rbind(data.frame(term="White", res = "1.00")) %>% 
                        rename(cHR_mort = res))

### Adjusted
hosp_coxph_surv <- map(modeling_data,
                  ~ coxph(Surv(time_from_event+0.1, cens_type==1)~Race + agegrp_at_event + SEX + zscore +
                            REGION + comorb_indx + time_on_dialysis, data = .) %>%
                    broom::tidy() %>%
                    filter(str_detect(term, 'Race')) %>%
                    select(term, estimate, p.value:conf.high) %>%
                    mutate(term = str_remove(term, 'Race')) %>%
                    mutate_at(vars(estimate, conf.low:conf.high), exp) %>% 
                    mutate(term = ifelse(term == 'Native American', 'AI/AN', term)) %>% 
                    mutate(res = as.character(glue("{round(estimate,2)} ({round(conf.low, 2)}, {round(conf.high,2)})"))) %>% 
                    select(term, res) %>% 
                    rbind(data.frame(term = "White", res = "1.00")) %>% 
                    rename(aHR_surv = res))

res_survival <- map2(out_crude_surv, hosp_coxph_surv, left_join)[c('stroke_primary','LuCa','dement','thrive')]

Perc <- map(modeling_data, ~group_by(., Race) %>% 
              summarize(perc = round(100*mean(cens_type==3, na.rm=T), 2)) %>% 
              ungroup() %>%
              mutate(Race = as.character(Race)) %>% 
              mutate(Race = ifelse(Race == "Native American", "AI/AN", Race)))[c('stroke_primary','LuCa','dement','thrive')]

tbl2 <- map2(Perc, res_discontinutation, left_join, by=c("Race" = "term")) %>% 
  map2(res_survival, left_join, by = c("Race" = "term")) %>% 
  map(~.x[match(race_order, .x$Race),]) %>% 
  bind_rows(.id = "Event") %>% 
  mutate(Event = events[Event]) %>% 
  clean_cols('Event')

# Table 3 -------------------------------------------------------------------------------------

load(file.path(dropdir, 'modeling_data.rda'))
modeling_data_young <- map(modeling_data, ~filter(., age_at_event < 70))
modeling_data_old <- map(modeling_data, ~filter(., age_at_event >= 70))

## Discontinuation
### Adjusted

hosp_coxph_young <- map(modeling_data_young,
                  ~ coxph(Surv(time_from_event+0.1, cens_type==3)~Race + agegrp_at_event + SEX + zscore +
                            REGION + comorb_indx + time_on_dialysis, data = .) %>%
                    broom::tidy() %>%
                    filter(str_detect(term, 'Race')) %>%
                    select(term, estimate, p.value:conf.high) %>%
                    mutate(term = str_remove(term, 'Race')) %>%
                    mutate_at(vars(estimate, conf.low:conf.high), exp) %>% 
                    mutate(term = ifelse(term == 'Native American', 'AI/AN', term)) %>% 
                    mutate(res = as.character(glue("{round(estimate,2)} ({round(conf.low, 2)}, {round(conf.high,2)})"))) %>% 
                    select(term, res) %>% 
                    rbind(data.frame(term = "White", res = "1.00")) %>% 
                    rename(aHR_disc = res))

hosp_coxph_old <- map(modeling_data_old,
                  ~ coxph(Surv(time_from_event+0.1, cens_type==3)~Race + agegrp_at_event + SEX + zscore +
                            REGION + comorb_indx + time_on_dialysis, data = .) %>%
                    broom::tidy() %>%
                    filter(str_detect(term, 'Race')) %>%
                    select(term, estimate, p.value:conf.high) %>%
                    mutate(term = str_remove(term, 'Race')) %>%
                    mutate_at(vars(estimate, conf.low:conf.high), exp) %>% 
                    mutate(term = ifelse(term == 'Native American', 'AI/AN', term)) %>% 
                    mutate(res = as.character(glue("{round(estimate,2)} ({round(conf.low, 2)}, {round(conf.high,2)})"))) %>% 
                    select(term, res) %>% 
                    rbind(data.frame(term = "White", res = "1.00")) %>% 
                    rename(aHR_disc = res))

## Survival
### Adjusted
hosp_surv_coxph_young <- map(modeling_data_young,
                  ~ coxph(Surv(time_from_event+0.1, cens_type==1)~Race + agegrp_at_event + SEX + zscore +
                            REGION + comorb_indx + time_on_dialysis, data = .) %>%
                    broom::tidy() %>%
                    filter(str_detect(term, 'Race')) %>%
                    select(term, estimate, p.value:conf.high) %>%
                    mutate(term = str_remove(term, 'Race')) %>%
                    mutate_at(vars(estimate, conf.low:conf.high), exp) %>% 
                    mutate(term = ifelse(term == 'Native American', 'AI/AN', term)) %>% 
                    mutate(res = as.character(glue("{round(estimate,2)} ({round(conf.low, 2)}, {round(conf.high,2)})"))) %>% 
                    select(term, res) %>% 
                    rbind(data.frame(term = "White", res = "1.00")) %>% 
                    rename(aHR_surv = res))

hosp_surv_coxph_old <- map(modeling_data_old,
                  ~ coxph(Surv(time_from_event+0.1, cens_type==1)~Race + agegrp_at_event + SEX + zscore +
                            REGION + comorb_indx + time_on_dialysis, data = .) %>%
                    broom::tidy() %>%
                    filter(str_detect(term, 'Race')) %>%
                    select(term, estimate, p.value:conf.high) %>%
                    mutate(term = str_remove(term, 'Race')) %>%
                    mutate_at(vars(estimate, conf.low:conf.high), exp) %>% 
                    mutate(term = ifelse(term == 'Native American', 'AI/AN', term)) %>% 
                    mutate(res = as.character(glue("{round(estimate,2)} ({round(conf.low, 2)}, {round(conf.high,2)})"))) %>% 
                    select(term, res) %>% 
                    rbind(data.frame(term = "White", res = "1.00")) %>% 
                    rename(aHR_surv = res))

# Simulation study

load(file.path(dropdir, 'cox_models_sim_strat.rda'))
bl_young <- modify_depth(cox_models_young, 2, ~select(., term, estimate) %>% 
                           mutate(estimate = exp(estimate))) %>% 
  map(~bind_rows(.) %>%
        mutate(term = str_remove(term, 'Race')) %>% 
        mutate(term = str_replace(term, 'Native American','AI/AN')) %>% 
        group_by(term) %>% 
        summarize(Min = min(estimate), Max = max(estimate)) %>% 
        mutate(sim_range = as.character(glue('({round(Min,2)},{round(Max,2)})'))) %>% 
        select(term, sim_range) %>% 
        rename(Race = term) %>% 
        rbind(data.frame(Race = 'White', sim_range = '1.00')))
bl_old <- modify_depth(cox_models_old, 2, ~select(., term, estimate) %>% 
                           mutate(estimate = exp(estimate))) %>% 
  map(~bind_rows(.) %>%
        mutate(term = str_remove(term, 'Race')) %>% 
        mutate(term = str_replace(term, 'Native American','AI/AN')) %>% 
        group_by(term) %>% 
        summarize(Min = min(estimate), Max = max(estimate)) %>% 
        mutate(sim_range = as.character(glue('({round(Min,2)},{round(Max,2)})'))) %>% 
        select(term, sim_range) %>% 
        rename(Race = term) %>% 
        rbind(data.frame(Race = 'White', sim_range = '1.00')))

N_young <- map(modeling_data_young, ~count(., Race) %>% 
                 mutate(Race = as.character(Race)) %>% 
                 mutate(Race = str_replace(Race, 'Native American', 'AI/AN')))
Perc_young = map(modeling_data_young, ~group_by(., Race) %>% 
             summarize(perc = round(100*mean(cens_type==3, na.rm=T),2)) %>% 
               mutate(Race = as.character(Race)) %>% 
               mutate(Race = str_replace(Race, 'Native American', 'AI/AN')))
N_old <- map(modeling_data_old, ~count(., Race) %>% 
                 mutate(Race = as.character(Race)) %>% 
                 mutate(Race = str_replace(Race, 'Native American', 'AI/AN')))
Perc_old = map(modeling_data_old, ~group_by(., Race) %>% 
             summarize(perc = round(100*mean(cens_type==3, na.rm=T),2)) %>% 
               mutate(Race = as.character(Race)) %>% 
               mutate(Race = str_replace(Race, 'Native American', 'AI/AN')))

res_young <- N_young %>% 
  map2(Perc_young, left_join) %>% 
  map2(hosp_coxph_young, left_join, by=c("Race" = "term")) %>%
  map2(hosp_surv_coxph_young, left_join, by=c("Race" = "term")) %>% 
  map2(bl_young, left_join) %>% 
  map(~.x[match(race_order,.x$Race),]) %>% 
  bind_rows(.id = 'Event') %>% 
  mutate(Event = events[Event]) %>% 
  filter(!is.na(Event))

res_old <- N_old %>% 
  map2(Perc_old, left_join) %>% 
  map2(hosp_coxph_old, left_join, by=c("Race" = "term")) %>%
  map2(hosp_surv_coxph_old, left_join, by=c("Race" = "term")) %>% 
  map2(bl_old, left_join)%>% 
  map(~.x[match(race_order,.x$Race),]) %>% 
  bind_rows(.id = 'Event') %>% 
  mutate(Event = events[Event]) %>% 
  filter(!is.na(Event))

tbl3 <- res_young %>% left_join(res_old, by = c("Event", 'Race')) %>% 
  clean_cols(Event)


openxlsx::write.xlsx(list('Table 1'= tab1, 'Table 2' = tbl2, 'Table 3' = tbl3), 
                     file = 'Tables.xlsx',
                     colWidths = 'auto')

# Figure 1 ------------------------------------------------------------------------------------


# Figure 2 ------------------------------------------------------------------------------------

cox_models <- readRDS(file.path(dropdir, 'cox_models.rds'))
cox_models <- cox_models[names(events)]
cox_models %>% modify_depth(2, ~select(., term, estimate) %>% 
                              mutate(term = str_remove(term, 'Race'),
                                     term = str_replace(term, 'Native American','AI/AN'),
                                     estimate = exp(estimate))) %>% 
  map(bind_rows) %>% 
  bind_rows(.id = 'Event') %>% 
  mutate(Event = events[Event]) %>% 
  mutate(Race = as.factor(term),
         Event = as.factor(Event)) %>% 
  mutate(Race = fct_relevel(Race, race_order[-1]),
         Event = fct_relevel(Event, c('Stroke','Lung cancer','Dementia', 'Failure to thrive'))) %>% 
  select(-term) -> bl

ggplot(bl, aes(x = estimate)) + geom_density() + 
  facet_grid(Event ~ Race, scales = 'free', switch = 'y') +
  geom_vline(xintercept = 1, linetype = 2) + 
  labs(x = 'Adjusted HR, compared to Whites', y = '') + 
  theme(strip.text = element_text(size = 14, face = 'bold'),
        strip.text.y = element_text(angle = 180),
        strip.background = element_rect(fill = 'white'),
        strip.placement = 'outside',
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.spacing.x = unit(2, 'lines'))
ggsave('Figure2.pdf', width = 12, height = 7)

ggplot(bl, aes(x = estimate)) + geom_density() + 
  facet_grid(Event ~ Race, scales = 'free', switch = 'y', space = 'free_x') +
  geom_vline(xintercept = 1, linetype = 2) + 
  labs(x = 'Adjusted HR, compared to Whites', y = '') + 
  theme(strip.text = element_text(size = 14, face = 'bold'),
        strip.text.y = element_text(angle = 180),
        strip.background = element_rect(fill = 'white'),
        strip.placement = 'outside',
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 8),
        panel.spacing.x = unit(2, 'lines'))
ggsave('Figure2a.pdf', width = 12, height = 7)
