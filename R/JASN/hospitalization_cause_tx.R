##%######################################################%##
#                                                          #
####                  Transplantation                   ####
#                                                          #
##%######################################################%##

# We've already computed a new survival time, surv_time2, that 
# incorporates loss-to-followup, death and withdrawal only. So 
# the clock doesn't stop for transplants. We'll see how this 
# affects things.


# setup -------------------------------------------------------------------

abhiR::reload()
dropdir <- path(find_dropbox(), 'NIAMS','Ward','USRDS2015','data')

condition_code <- c('stroke_primary' = 'Primary Stroke',
                    'stroke_compl' = 'Complicated Stroke',
                    'LuCa' = 'Lung Cancer',
                    'MetsCa' = 'Metastatic Cancer',
                    'dement' = 'Dementia',
                    'thrive' = 'Failure to thrive'
)

# Transplant frequency in each condition group ----------------------------

hosp_post_dx <- readRDS(path(dropdir, 'revision_JASN', 'final_hosp_data.rds'))

tx_freq <- map(hosp_post_dx, 
               ~count(., cens_type) %>% 
                 mutate(Percentage = 100*n/sum(n)) %>% 
                 select(-n) %>% 
                 filter(cens_type == 2)) %>% 
  bind_rows(.id = 'Condition') %>% 
  mutate(Condition = condition_code[Condition]) %>% 
  mutate(Percentage = round(Percentage, 2))


# munge data for modeling -------------------------------------------------

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
    select(time_from_event, time_from_event2, cens_type, age_at_event, agegrp_at_event, Race, Age = age_cat, Sex = SEX, Region = REGION, zscore,
           comorb_indx,
           time_on_dialysis, ESRD_Cause)
  
}

events <- c('stroke_primary' = 'Stroke',
            'LuCa' = "Lung cancer",
            'dement' = "Dementia",
            'thrive' = 'Failure to thrive')

# load(path(dropdir, 'revision_JASN','modeling_data.rda'))
# munged_modeling <- map(modeling_data, munge_data)
# save(munged_modeling, file = path(dropdir, 'revision_JASN', 'munged_modeling.rda'), compress = T)

load(path(dropdir, 'revision_JASN','munged_modeling.rda'))
# Crude & adjusted models for discontinuation -----------------------------
fit_list = map(munged_modeling,  ~survfit(Surv(time_from_event2, cens_type==3)~ Race,
                                          data = .))
cph1_list <-  map(munged_modeling, ~ coxph(Surv(time_from_event2, cens_type==3)~ Race,
                                           data = .))
out_crude <- map(cph1_list, ~tidy(.) %>% select(term, estimate, starts_with('conf')) %>%
                   mutate_if(is.numeric, exp) %>%
                   mutate(term = str_remove(term, 'Race')) %>%
                   mutate(res = as.character(
                     glue('{round(estimate,2)} ({round(conf.low, 2)}, {round(conf.high,2)})'))) %>%
                   select(term, res) %>%
                   mutate(term = ifelse(term == "Native American", 'AI/AN', term)) %>%
                   add_row(term = "White", res = '1.00 (ref)') %>% 
                   rename(cHR_disc = res))


hosp_coxph <- map(munged_modeling,
                  ~ coxph(Surv(time_from_event2+0.1, cens_type==3)~Race + agegrp_at_event + Sex + zscore +
                            Region + comorb_indx + time_on_dialysis + ESRD_Cause, data = .) %>%
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



# Crude & adjusted models for mortality -----------------------------------

cph1_list_surv <-  map(munged_modeling, ~ coxph(Surv(time_from_event2, cens_type %in% c(1,3))~ Race,
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
                       ~ coxph(Surv(time_from_event2+0.1, cens_type %in% c(1,3))~Race + agegrp_at_event + Sex + zscore +
                                 Region + comorb_indx + time_on_dialysis + ESRD_Cause, data = .) %>%
                         broom::tidy() %>%
                         filter(str_detect(term, 'Race')) %>%
                         select(term, estimate, p.value:conf.high) %>%
                         mutate(term = str_remove(term, 'Race')) %>%
                         mutate_at(vars(estimate, conf.low:conf.high), exp) %>%
                         mutate(term = ifelse(term == 'Native American', 'AI/AN', term)) %>%
                         mutate(res = as.character(glue("{round(estimate,2)} ({round(conf.low, 2)}, {round(conf.high,2)})"))) %>%
                         select(term, res) %>%
                         rbind(data.frame(term = "White", res = "1.00 (ref)")) %>%
                         rename(aHR_mort = res))
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

openxlsx::write.xlsx(list('Transplant Frequency' = tx_freq, 'Table 2' = tbl2), file = 'results/revision_JASN/no_transplant_results.xlsx',
                     creator = 'Abhijit Dasgupta',
                     colWidths = 'auto')
