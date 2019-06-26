##%######################################################%##
#                                                          #
####    Tables and figures for hospitalization study    ####
#                                                          #
##%######################################################%##


# setup ---------------------------------------------------------------------------------------

ProjTemplate::reload()
# dbdir = verifyPaths(); dir.exists(dbdir)
dropdir <- file.path(ProjTemplate::find_dropbox(), 'NIAMS','Ward','USRDS2015','data')
load(file.path(dropdir, 'modeling_data.rda'))

## Munge modeling data once here for all the tables

munge_data <- function(d){
  out <- d %>%
    mutate(age_cat = case_when(
      INC_AGE <=49 ~ '18-49',
      INC_AGE %in% seq(50,59) ~ '50-59',
      INC_AGE %in% seq(60,69) ~ '60-69',
      INC_AGE %in% seq(70,79) ~ '70-79',
      INC_AGE >= 80 ~ '80+'
    ),
    REGION = factor(REGION, levels = c('Northeast','South','Midwest','West')),
    SEX = factor(ifelse(SEX == '1','Male','Female'),levels = c('Male','Female')),
    time_on_dialysis = as.numeric(time_on_dialysis)/30.42) %>% # convert into months
    mutate(Race = fct_relevel(Race,
                              c("White", "Black",'Hispanic','Asian','Native American')) %>%
             fct_recode(c("AI/AN" = 'Native American'))) %>%
    mutate(agegrp_at_event = fct_recode(agegrp_at_event,
                                        '<50' = '<40',
                                        '<50' = '[40,50)')) %>% 
    select(time_from_event, cens_type, age_at_event, agegrp_at_event, Race, Age = age_cat, Sex = SEX, Region = REGION, zscore,
            comorb_indx,
           time_on_dialysis)
  
}

events <- c('stroke_primary' = 'Stroke',
            'LuCa' = "Lung cancer",
            'dement' = "Dementia",
            'thrive' = 'Failure to thrive')

munged_modeling <- map(modeling_data, munge_data)
save(munged_modeling, file = file.path(dropdir, 'munged_modeling.rda'), compress = T)

# Table 1 -------------------------------------------------------------------------------------

getTable1 <- function(d){
  out <- d %>%
    select(Race:time_on_dialysis) %>% 
    rename(`SES score` = zscore,
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


tab1 <- map(munged_modeling[c('stroke_primary','LuCa','dement','thrive')], getTable1)
names(tab1) <- c('Stroke', 'Lung cancer', 'Dementia', "Failure to thrive")
tab1 <- tab1 %>% bind_rows(.id = 'Event') %>%
  clean_cols(Event) %>%
  add_blank_rows(.before = which(.$Event != '')[-1]) %>%
  mutate_all(~replace_na(., ''))


# Table 2 -------------------------------------------------------------------------------------


## Discontinuation

### Crude
fit_list = map(munged_modeling,  ~survfit(Surv(time_from_event, cens_type==3)~ Race,
                                       data = .))
cph1_list <-  map(munged_modeling, ~ coxph(Surv(time_from_event, cens_type==3)~ Race,
                                         data = .))
# for(i in 1:6){
#   plt_list[[i]] <- survMisc::autoplot(fit_list[[i]], type='single', censSize = 0,
#                                       title = names(fit_list)[i],
#                                       xLab = 'Days from index event',
#                                       legTitle = '')$plot +
#     scale_y_continuous('Percent who discontinued dialysis', labels = scales::percent)+
#     annotate('text', x = 50, y = 0.1, label=paste0('p-value : ', logrank_list[[i]]),hjust=0) +
#     theme(legend.position = 'bottom', legend.justification = c(0.5, 0.5))
# }

out_crude <- map(cph1_list, ~tidy(.) %>% select(term, estimate, starts_with('conf')) %>%
                   mutate_if(is.numeric, exp) %>%
                   mutate(term = str_remove(term, 'Race')) %>%
                   mutate(res = as.character(
                     glue('{round(estimate,2)} ({round(conf.low, 2)}, {round(conf.high,2)})'))) %>%
                   select(term, res) %>%
                   mutate(term = ifelse(term == "Native American", 'AI/AN', term)) %>%
                   add_row(term = "White", res = '1.00 (ref)') %>% 
                   rename(cHR_disc = res))
### Adjusted
hosp_coxph <- map(munged_modeling,
                  ~ coxph(Surv(time_from_event+0.1, cens_type==3)~Race + agegrp_at_event + Sex + zscore +
                            Region + comorb_indx + time_on_dialysis, data = .) %>%
                    broom::tidy() %>%
                    filter(str_detect(term, 'Race')) %>%
                    select(term, estimate, p.value:conf.high) %>%
                    mutate(term = str_remove(term, 'Race')) %>%
                    mutate_at(vars(estimate, conf.low:conf.high), exp) %>%
                    mutate(res = as.character(glue("{round(estimate,2)} ({round(conf.low, 2)}, {round(conf.high,2)})"))) %>%
                    select(term, res) %>%
                    add_row(term='White', res = '1.00 (ref)', .before = 1) %>% 
                    rename(aHR_disc = res))

res_discontinutation <- map2(out_crude, hosp_coxph, left_join)
res_discontinutation <-  res_discontinutation[c('stroke_primary','LuCa','dement','thrive')]

## Survival

cph1_list_surv <-  map(munged_modeling, ~ coxph(Surv(time_from_event, cens_type %in% c(1,3))~ Race,
                                         data = .))
out_crude_surv <- map(cph1_list_surv,
                      ~tidy(.) %>% select(term, estimate, starts_with('conf')) %>%
                        mutate_if(is.numeric, exp) %>%
                        mutate(term = str_remove(term, 'Race')) %>%
                        mutate(res = as.character(
                          glue('{round(estimate,2)} ({round(conf.low, 2)}, {round(conf.high,2)})'))) %>%
                        select(term, res) %>%
                        mutate(term = ifelse(term == "Native American", 'AI/AN', term)) %>%
                        rbind(data.frame(term="White", res = "1.00 (ref)")) %>%
                        rename(cHR_mort = res))

### Adjusted
hosp_coxph_surv <- map(munged_modeling,
                  ~ coxph(Surv(time_from_event+0.1, cens_type %in% c(1,3))~Race + agegrp_at_event + Sex + zscore +
                            Region + comorb_indx + time_on_dialysis, data = .) %>%
                    broom::tidy() %>%
                    filter(str_detect(term, 'Race')) %>%
                    select(term, estimate, p.value:conf.high) %>%
                    mutate(term = str_remove(term, 'Race')) %>%
                    mutate_at(vars(estimate, conf.low:conf.high), exp) %>%
                    mutate(term = ifelse(term == 'Native American', 'AI/AN', term)) %>%
                    mutate(res = as.character(glue("{round(estimate,2)} ({round(conf.low, 2)}, {round(conf.high,2)})"))) %>%
                    select(term, res) %>%
                    rbind(data.frame(term = "White", res = "1.00 (ref)")) %>%
                    rename(aHR_surv = res))

res_survival <- map2(out_crude_surv, hosp_coxph_surv, left_join)[c('stroke_primary','LuCa','dement','thrive')]

Perc <- map(munged_modeling, ~group_by(., Race) %>%
              summarize(perc = round(100*mean(cens_type==3, na.rm=T), 2)) %>%
              ungroup() %>%
              mutate(Race = as.character(Race)) %>%
              mutate(Race = ifelse(Race == "Native American", "AI/AN", Race)))[c('stroke_primary','LuCa','dement','thrive')]

race_order <- c('White','Black','Hispanic','Asian','AI/AN')

tbl2 <- map2(Perc, res_discontinutation, left_join, by=c("Race" = "term")) %>%
  map2(res_survival, left_join, by = c("Race" = "term")) %>%
  map(~.x[match(race_order, .x$Race),]) %>%
  bind_rows(.id = "Event") %>%
  mutate(Event = events[Event]) %>%
  clean_cols('Event') %>%
  add_blank_rows(.before = which(.$Event != '')[-1]) %>%
  mutate_all(~replace_na(., ''))


# Table 3 -------------------------------------------------------------------------------------
munged_modeling_young <- map(munged_modeling, ~filter(., age_at_event < 70) %>% 
                               mutate(agegrp_at_event = fct_drop(agegrp_at_event)))
munged_modeling_old <- map(munged_modeling, ~filter(., age_at_event >= 70) %>% 
                               mutate(agegrp_at_event = fct_drop(agegrp_at_event)))

## Discontinuation
### Adjusted

hosp_coxph_young <- map(munged_modeling_young,
                  ~ coxph(Surv(time_from_event+0.1, cens_type==3)~Race + agegrp_at_event + Sex + zscore +
                            Region + comorb_indx + time_on_dialysis, data = .) %>%
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

hosp_coxph_old <- map(munged_modeling_old,
                  ~ coxph(Surv(time_from_event+0.1, cens_type==3)~Race + agegrp_at_event + Sex + zscore +
                            Region + comorb_indx + time_on_dialysis, data = .) %>%
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
hosp_surv_coxph_young <- map(munged_modeling_young,
                  ~ coxph(Surv(time_from_event, cens_type %in% c(1,3))~Race + agegrp_at_event + Sex + zscore +
                            Region + comorb_indx + time_on_dialysis, data = .) %>%
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

hosp_surv_coxph_old <- map(munged_modeling_old,
                  ~ coxph(Surv(time_from_event, cens_type %in% c(1,3))~Race + agegrp_at_event + Sex + zscore +
                            Region + comorb_indx + time_on_dialysis, data = .) %>%
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
        add_row(Race = 'White', sim_range = '1.00', .before = 1))

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
        add_row(Race = 'White', sim_range = '1.00', .before = 1))

N_young <- map(munged_modeling_young, ~count(., Race) %>%
                 mutate(Race = as.character(Race)))
Perc_young = map(munged_modeling_young, ~group_by(., Race) %>%
             summarize(perc = round(100*mean(cens_type==3, na.rm=T),2)) %>%
               mutate(Race = as.character(Race)))
N_old <- map(munged_modeling_old, ~count(., Race) %>%
                 mutate(Race = as.character(Race)))
Perc_old = map(munged_modeling_old, ~group_by(., Race) %>%
             summarize(perc = round(100*mean(cens_type==3, na.rm=T),2)) %>%
               mutate(Race = as.character(Race)))

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
  clean_cols(Event) %>% 
  set_names(str_replace(names(.), '\\.x','_young')) %>% 
  set_names(str_replace(names(.), '\\.y','_old')) %>% 
  add_blank_rows(.before = which(.$Event != '')[-1]) %>% 
  mutate_all(~replace_na(.,''))




# Supplementary Table 1 -----------------------------------------------------------------------

load(file.path(dropdir, 'munged_modeling.rda'))
race_order <- c("White",'Black','Hispanic','Asian','AI/AN')
events <- c('stroke_primary' = 'Stroke',
            'LuCa' = "Lung cancer",
            'dement' = "Dementia",
            'thrive' = 'Failure to thrive')
lbls <- c('Race' = 'Race', 'agegrp_at_event' = 'Age',  'Sex'  = 'Gender',
          'zscore' = 'SES Score', 'Region' = 'Region', 'comorb_indx' = 'Comorbidity',
          'time_on_dialysis' = 'Time on Dialysis')


bl <- munged_modeling[c('stroke_primary','LuCa','dement','thrive')]
bl <- map(bl, ~mutate(., zscore = zscore/10)) # Rescale zscore to 10 units
                  
hosp_coxph_surv <- map(bl,
                       ~ coxph(Surv(time_from_event+0.1, cens_type %in% c(1,3))~Race + agegrp_at_event + Sex + zscore +
                                 Region + comorb_indx + time_on_dialysis, data = .) )

res <-  map(hosp_coxph_surv, table_results.coxph, lbls = lbls,tidy = F, dig = 3)
for (nm in names(res)) {
  res[[nm]] <- res[[nm]] %>% set_names(c('Variable',events[nm]))
}
results <- Reduce(left_join, res) %>% 
  mutate(Variable = format_interval(Variable))

## Adding full model for discontinuation
hosp_coxph_disc <- map(bl,
                  ~ coxph(Surv(time_from_event+0.1, cens_type==3)~Race + agegrp_at_event + Sex + zscore +
                            Region + comorb_indx + time_on_dialysis, data = .)) 
res_disc <- map(hosp_coxph_disc, table_results.coxph, lbls=lbls, tidy=F, dig = 3)
for (nm in names(res_disc)){
  res_disc[[nm]] <- res_disc[[nm]] %>% set_names(c('Variable',events[nm]))
  
}
res_disc <- Reduce(left_join, res_disc) %>% mutate(Variable = format_interval(Variable))

openxlsx::write.xlsx(list('Table 1'= tab1, 'Table 2' = tbl2, 'Table 3' = tbl3,
                          'Supplementary Table 1' = results,
                          'Supplementary Table 1a' = res_disc),
                     file = 'Tables.xlsx',
                     colWidths = 'auto',
                     creator = "Abhijit Dasgupta")

# Figure 1 ------------------------------------------------------------------------------------

library(survival)
library(survminer)
load(file.path(dropdir, 'munged_modeling.rda'))

d <- bind_rows(munged_modeling, .id = 'Event') %>%
  mutate(Event = events[Event]) %>%
  filter(!is.na(Event)) %>%
  # mutate(Race  = str_replace(Race, 'Native American','AI/AN')) %>%
  mutate_at(vars(Event, Race), as.factor) %>%
  mutate(Event = fct_relevel(Event, c('Stroke','Lung cancer','Dementia','Failure to thrive')),
         Race = fct_relevel(Race, c('White','Black','Hispanic','Asian','AI/AN')))

bl <- d %>% nest(-Event) %>%
  mutate(mods = map(data, ~survfit(Surv(time_from_event, cens_type == 3) ~ Race, data = .))) %>%
  mutate(survdata = map(mods, ~broom::tidy(.) %>% mutate(strata = str_remove(strata,'Race=')) %>% 
                          mutate(strata = factor(strata, levels = c('White','Black','Hispanic','Asian','AI/AN'))) %>% 
                          rename(Race = strata) %>% 
                          filter(time <= 750))) %>% # Restrict to 750 days (2 years) 
  mutate(plots = map(survdata, ~ggplot(., aes(x = time, y = 1-estimate, color=Race)) +
                       geom_step() + 
                       scale_x_continuous(breaks = seq(0,750, by=150))+
                       scale_y_continuous(breaks = seq(0, 0.5, by=0.1), limits= c(0, 0.5))+
                       labs(x = '', y = '') +
                       theme(axis.text.x = element_blank(),
                             axis.ticks.x = element_blank(),
                             legend.position = 'none',
                             axis.text.y = element_text(size=9))))

legend_b <- ggdraw(plot_grid(NULL,
                             get_legend(bl$plots[[4]] +
                         theme(legend.position='bottom', 
                               legend.title = element_text(size = 12, face = 'bold'), 
                               legend.text = element_text(size = 10))),
                         NULL, 
                         nrow = 1))
bl$plots[[4]] <- bl$plots[[4]]  + theme(axis.ticks.x = element_line(),
                                        axis.text.x = element_text(size = 10))

plt_disc <- plot_grid(plotlist = bl$plots, ncol = 1, align = 'v', rel_heights = c(1,1,1,1.2),
          labels = levels(d$Event), label_size = 10, label_x = 0.25, vjust = 1, hjust = 0)
+
  draw_label('Probability', x = 0.02, y = 0.5, angle = 90, hjust = 0.2, vjust = 1)
title = ggdraw() + draw_label('Discontinuation', fontface = 'bold')
bottom = ggdraw() + draw_label('Days from event', size = 12)

plt_disc_complete <- plot_grid(title, plt_disc, ncol = 1, rel_heights = c(0.1, 1))

bl2 <- d %>%
  nest(-Event) %>%
  mutate(mods = map(data, ~survfit(Surv(time_from_event, cens_type %in% c(1,3)) ~ Race, data = .))) %>%
  mutate(survdata = map(mods, ~broom::tidy(.) %>% mutate(strata = str_remove(strata, 'Race=')) %>% 
                          filter(time <= 750) %>% # Restrict time to 750 days
                          mutate(strata = factor(strata, levels = c('White','Black','Hispanic','Asian','AI/AN'))))) %>% 
  mutate(plots = map(survdata, ~ggplot(., aes(x = time, y = estimate, color=strata)) +
                       geom_step() + 
                       scale_x_continuous(breaks = seq(0,750,by=150))+
                       scale_y_continuous(breaks = seq(0, 1, by=0.25), limits = c(0,1)) +
                       labs(x = '', y = '') +
                       theme(axis.text.x = element_blank(),
                             axis.ticks.x = element_blank(),
                             legend.position = 'none',
                             axis.text.y = element_text(size=9))))

bl2$plots[[4]] <- bl2$plots[[4]] +
  theme(axis.ticks.x = element_line(), axis.text.x = element_text(size = 10))
plt_surv <- plot_grid(plotlist = bl2$plots, ncol = 1, align = 'v', rel_heights = c(1,1,1,1.2),
                      labels = levels(d$Event), label_size = 10, label_x = 0.3, vjust = 1, hjust = 0)
title2 = ggdraw() + draw_label('Mortality', fontface = 'bold')
plt_surv_complete = plot_grid(title2, plt_surv, ncol = 1, rel_heights = c(0.1, 1))

plt1 <- plot_grid(plt_disc_complete, plt_surv_complete, nrow=1, rel_widths = c(0.95, 1)) + 
  draw_label('Probability', x = 0.02, y = 0.5, angle = 90, hjust = 0.2, vjust = 0.3)

plt <- plt1 %>% 
  plot_grid(bottom, ncol = 1, rel_heights = c(1,0.05)) %>%
  plot_grid(legend_b, ncol = 1, rel_heights = c(1, 0.1))
ggsave('graphs/Figure1.pdf', width = 6, height = 8)

# Figure 2 ------------------------------------------------------------------------------------

load(file.path(dropdir, 'munged_modeling.rda'))
race_order <- c("White",'Black','Hispanic','Asian','AI/AN')

cox_models <- readRDS(file.path(dropdir, 'cox_models.rds'))

cox_models <- cox_models[names(events)]
bl <- cox_models %>% modify_depth(2, ~select(., term, estimate) %>%
                              mutate(term = str_remove(term, 'Race'),
                                     term = str_replace(term, 'Native American','AI/AN'),
                                     estimate = exp(estimate))) %>%
  map(bind_rows) %>% # Collapse the internal list
  bind_rows(.id = 'Event') %>% # Collapse the external list
  mutate(Event = events[Event]) %>%
  mutate(Race = as.factor(term),
         Event = as.factor(Event)) %>%
  mutate(Race = fct_relevel(Race, race_order[-1]),
         Event = fct_relevel(Event, c('Stroke','Lung cancer','Dementia', 'Failure to thrive'))) %>%
  select(-term) 


# ggplot(bl, aes(x = estimate)) + geom_density() +
#   facet_grid(Event ~ Race, scales = 'free', switch = 'y') +
#   geom_vline(xintercept = 1, linetype = 2) +
#   labs(x = 'Adjusted HR, compared to Whites', y = '') +
#   theme(strip.text = element_text(size = 14, face = 'bold'),
#         strip.text.y = element_text(angle = 180),
#         strip.background = element_rect(fill = 'white'),
#         strip.placement = 'outside',
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         panel.spacing.x = unit(2, 'lines'))
# ggsave('Figure2.pdf', width = 12, height = 7)

bl2 <- map(munged_modeling,
           ~ coxph(Surv(time_from_event+0.1, cens_type %in% c(1,3))~Race + agegrp_at_event + Sex + zscore +
                     Region + comorb_indx + time_on_dialysis, data = .) %>%
             broom::tidy() %>%
             filter(str_detect(term, 'Race')) %>%
             select(term, estimate)) %>% 
  bind_rows(.id = 'Event') %>% 
  filter(Event %in% c('stroke_primary','LuCa', 'dement', 'thrive')) %>% 
  mutate(estimate = exp(estimate),
         term = str_remove(term, 'Race')) %>% 
  mutate(Event = as.factor(Event),
         Race = as.factor(term))  %>% 
  mutate(Race = fct_relevel(Race, race_order[-1]),
         Event = fct_recode(Event, 'Stroke'='stroke_primary','Lung cancer' = 'LuCa',
                                      'Dementia' = 'dement', 'Failure to thrive' = 'thrive'),
         Event = fct_relevel(Event, c('Stroke','Lung cancer','Dementia','Failure to thrive'))) %>%
  select(-term) %>% 
  mutate(hts = case_when(Event == 'Stroke' ~ 600, Event == 'Lung cancer' ~ 200, Event == 'Dementia' ~ 500, Event == 'Failure to thrive' ~ 600)) %>% 
  mutate(hts = hts / 4)

ggplot(rbind(bl, data.frame('Event'='Lung cancer', 'estimate' = 1.2, 'Race'='Asian')), aes(x = estimate)) + 
  geom_histogram(bins = 50, fill = 'black')+#geom_density() +
  facet_grid(Event ~ Race, scales = 'free', switch = 'y', space = 'free_x')+
  geom_vline(xintercept = 1, linetype = 2) +
  #geom_point(data = bl2, aes(x = estimate, y = 1), color='red', size = 4)+
  # geom_segment()
  #geom_vline(data = bl2, aes(xintercept = estimate), linetype = 2, color='red') +
  geom_segment(data = bl2, aes(x = estimate, xend=estimate, yend = 5, y = hts),
               color = 'grey50', size = 1.5, arrow = arrow(length = unit(.2, 'cm')))+
  scale_x_continuous(breaks = c(1, seq(0.4, 1.6, by = 0.2)))+ # Unified the x-axis ticks
  labs(x = 'Adjusted HR, compared to Whites', y = '') +
  theme(strip.text = element_text(size = 14, face = 'bold'),
        strip.text.y = element_text(angle = 180), # Rotate the y-axis labels
        strip.background.x = element_rect(fill = 'white'),
        strip.placement = 'outside', # Move labels outside the borders
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = 8),
        panel.spacing.x = unit(2, 'lines'))
ggsave('graphs/Figure2a.pdf', width = 12, height = 7)



