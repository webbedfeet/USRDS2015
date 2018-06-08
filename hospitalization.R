##%######################################################%##
#                                                          #
####             Script to interrogate the              ####
####           hospital databases to extract            ####
####                IDs # of individuals                ####
####       who had prevalent comorbidities. This        ####
####      will be matched against # IDs of people       ####
####           discontinuing dialysis to see            ####
####           how the race pattern appears.            ####
#                                                          #
##%######################################################%##


# setup ---------------------------------------------------------------------------------------


ProjTemplate::reload()
dbdir = verifyPaths(); dir.exists(dbdir)
dropdir <- file.path(ProjTemplate::find_dropbox(), 'NIAMS','Ward','USRDS2015','data')

# extract data from DB ------------------------------------------------------------------------


sql_conn = dbConnect(SQLite(), file.path(dbdir,'USRDS.sqlite3'))

till2009 <- tbl(sql_conn, 'till2009')
from2010 <- tbl(sql_conn, 'from2010')
studyids <- tbl(sql_conn, 'StudyIDs')
dbs <- list(till2009, from2010)

# Stroke ------------------------------------------------------------------

stroke <- dbs %>%
  lapply(., function(db){
    db %>%
      select(USRDS_ID, starts_with('HSDIAG'), CLM_FROM, CLM_THRU)%>%
      mutate(PRIM = substr(HSDIAG1,1,3)) %>%
      filter(PRIM == '430' | PRIM=='431' | PRIM == '432' | PRIM == '433' | PRIM=='434') %>%
      inner_join(studyids) %>% # Keep only individuals in earlier study
      collect(n = Inf)
  })

stroke_primary <- stroke %>%
  lapply(., function(db) db %>%  select(USRDS_ID, CLM_FROM, CLM_THRU)) %>% bind_rows() %>% distinct() %>% 
  mutate(dt = date_midpt(CLM_FROM, CLM_THRU))
head(stroke_primary)
# Stroke with complications -----------------------------------------------

stroke_compl <- stroke %>%
  lapply(., function(db){
    db %>% gather(diag, code, -USRDS_ID, -HSDIAG1, -CLM_THRU, -CLM_FROM) %>%
      mutate(code1 = substr(code,1,3)) %>%
      filter(code1 %in% c('438','342','344')) %>%
      select(USRDS_ID, CLM_FROM, CLM_THRU) %>%
      distinct()
  }) %>%
  bind_rows() %>%
  distinct() %>% mutate(dt = date_midpt(CLM_FROM,CLM_THRU))

head(stroke_compl)

# Lung cancer -------------------------------------------------------------

LuCa <- dbs %>%
  lapply(., function(db){
    db %>% mutate(PRIM = substr(HSDIAG1,1,3)) %>%
      filter(PRIM=='162') %>% 
      inner_join(studyids) %>% # Keep only individuals in earlier study
      select(USRDS_ID, CLM_FROM, CLM_THRU) %>% collect(n=Inf)
  }) %>%
  bind_rows() %>% distinct() %>% 
  mutate(dt = date_midpt(CLM_FROM, CLM_THRU))
head(LuCa)
# Metastatic cancer -------------------------------------------------------

MetsCa <- dbs %>%
  lapply(., function(db){
  db %>% mutate(PRIM=substr(HSDIAG1,1,3)) %>%
    filter(PRIM== '196' | PRIM == '197' | PRIM == '198' | PRIM == '199') %>%
      inner_join(studyids) %>% 
    select(USRDS_ID, CLM_FROM, CLM_THRU) %>% collect(n = Inf)}) %>%
  bind_rows() %>%
  distinct() %>% mutate(dt = date_midpt(CLM_FROM, CLM_THRU))
head(MetsCa)

hospitalization <- list('stroke_primary' = stroke_primary,
                        'stroke_compl' = stroke_compl,
                        'LuCa' = LuCa,
                        'MetsCa' = MetsCa)
                        # 'dement' = dement,
                        # 'thrive' = thrive
saveRDS(hospitalization, file = 'data/hospitalization_ids.rds', compress = T)

# Dementia ----------------------------------------------------------------

## I'm moving this to Python since it's much faster at processing the database
## row-wise. However, I need to get some SQL calls generated here.

# till2009 %>% select(USRDS_ID, starts_with('HSDIAG')) %>% show_query()
# from2010 %>% select(USRDS_ID, starts_with("HSDIAG")) %>% show_query()
# reticulate::source_python('dementia.py')

# dementia <- read_csv('data/Dementia.csv')
# names(dementia) <- 'USRDS_ID'
# head(dementia)

StudyIDS <- studyids %>% collect(n=Inf)
sql1 <- paste(capture.output(till2009 %>%
                               select(USRDS_ID, starts_with('HSDIAG'), CLM_FROM, CLM_THRU) %>%
                               show_query(), type='message')[-1], collapse=' ')
sql2 <- paste(capture.output(from2010 %>%
                               select(USRDS_ID, starts_with('HSDIAG'), CLM_FROM, CLM_THRU) %>%
                               show_query(), type='message')[-1], collapse=' ')
sqlist <- list(sql1,sql2)


dement <- list()
i=0
for (sql in sqlist){
  print(paste('Running ',sql))
  rs <- dbSendQuery(sql_conn, sql)
  while(!dbHasCompleted(rs)){
    d <-  dbFetch(rs, n = 100000)
    i=i+1
    print(i)
    dement[[i]] <-d %>% gather(hsdiag, code, -USRDS_ID, -CLM_FROM, -CLM_THRU) %>%
                   filter(str_detect(code, '^290|^2941|^331[012]')) %>%
                   select(USRDS_ID, CLM_FROM, CLM_THRU) %>%
                   # distinct() %>% inner_join(StudyIDS) %>% 
                   as.data.frame()
  }
  dbClearResult(rs)
}

dement1 <- bind_rows(dement)
dement1 <- dement1 %>% semi_join(StudyIDS)

hospitalization[['dement']] <- dement1
saveRDS(hospitalization, file = 'data/hospitalization_ids.rds', compress = T)

# Failure to thrive -------------------
## This can appear in any of the diagnoses

sql1 <- paste(capture.output(till2009 %>%
                               select(USRDS_ID, starts_with('HSDIAG'), CLM_FROM, CLM_THRU) %>%
                         show_query(), type='message')[-1], collapse=' ')
sql2 <- paste(capture.output(from2010 %>%
                               select(USRDS_ID, starts_with('HSDIAG'), CLM_FROM, CLM_THRU) %>%
                               show_query(), type='message')[-1], collapse=' ')
sqlist <- list(sql1,sql2)

thrive = list()
i = 0
for (sql in sqlist) {
  rs <- dbSendQuery(sql_conn, sql)
  while (!dbHasCompleted(rs)) {
    d <- dbFetch(rs, n = 100000)
    i = i+1
    print(i)
    thrive[[i]] <-d %>% gather(hsdiag, code, starts_with("HSDIAG")) %>%
                  filter(str_detect(code, '783[237]')) %>%
                  select(USRDS_ID, CLM_FROM, CLM_THRU) %>%
  				  as.data.frame()
  }
  dbClearResult(rs)
}

thrive1 <- bind_rows(thrive) %>% semi_join(StudyIDS)
hospitalization[['thrive']] <- thrive1


saveRDS(hospitalization, file = 'data/hospitalization_ids.rds')
saveRDS(hospitalization, file = file.path(dropdir, 'hospitalization_ids.rds'))

dbDisconnect(sql_conn); gc()



# Filter index conditions by events after start of dialysis -----------------------------------
# IDEA: We could also look at individuals who had index condition soon followed by dialysis,
# where dialysis is the precipitating index condition

ProjTemplate::reload()
hospitalization <- readRDS('data/hospitalization_ids.rds')
Dat <- readRDS('data/rda/Analytic.rds')
Dat <- Dat %>% mutate(surv_date = pmin(cens_time, withdraw_time, DIED, TX1DATE, na.rm=T)) %>% 
  mutate(RACE2 = forcats::fct_relevel(RACE2, 'White'))

hosp_post_dx <- 
  map(hospitalization, ~ .x %>% 
        mutate(CLM_FROM = as.Date(CLM_FROM)) %>% 
        left_join(Dat %>% select(USRDS_ID, FIRST_SE, surv_date, cens_type, RACE2)) %>% 
        filter(CLM_FROM >= FIRST_SE, CLM_FROM <= surv_date) %>% 
        group_by(USRDS_ID) %>% 
        top_n(-1, CLM_FROM) %>% 
        top_n(-1, CLM_THRU) %>% 
        # select(-FIRST_SE, -surv_date) %>% 
        distinct() %>% 
        ungroup())

saveRDS(hosp_post_dx, 'data/rda/final_hosp_data.rds', compress = T)
saveRDS(hosp_post_dx, file.path(dropdir, 'final_hosp_data.rds'), compress = T)

# Propensity of withdrawal based on timing of Comorb ------------------------------------------

#' what's the chance of discontinuation, by race
hosp_post_dx <- readRDS(file.path(dropdir,'final_hosp_data.rds'))
Dat <- readRDS('data/rda/Analytic.rds')
Dat <- Dat %>% mutate(surv_date = pmin(cens_time, withdraw_time, DIED, TX1DATE, na.rm=T)) %>% 
  mutate(RACE2 = forcats::fct_relevel(RACE2, 'White'))

hosp_postdx_age <- map(
  hosp_post_dx,
  ~.x %>% left_join(Dat %>% select(USRDS_ID, INC_AGE)) %>% 
    mutate(se_to_event_time = CLM_FROM - FIRST_SE,
           age_at_event = floor(INC_AGE + se_to_event_time/365.25))  %>% 
    mutate(agegrp_at_event = cut_width(age_at_event, width=10, boundary=10, closed='left')) %>% 
    mutate(agegrp_at_event = 
             forcats::fct_collapse(agegrp_at_event,
                                   '<40' = intersect(levels(agegrp_at_event),c('[10,20)','[20,30)','[30,40)')),
                                   '80+' = intersect(levels(agegrp_at_event),
                                                     c('[80,90)','[90,100)','[90,100]','[100,110]')))))

out1 <- map(hosp_postdx_age, ~.x %>% 
      group_by(agegrp_at_event, RACE2) %>% 
      summarize(prop_withdrew = round(mean(cens_type==3), 3), N = n()) %>% 
      ungroup()) %>% 
  bind_rows(.id = 'index_event') %>% 
  filter(!is.na(RACE2)) %>% 
  unite(out, c('prop_withdrew','N'), sep=' / ') %>% 
  spread(agegrp_at_event, out) %>% 
  mutate(index_event = transform_indx(index_event)) %>% 
 rename(`Index event`=index_event, Race=RACE2)

out2 <- map(hosp_post_dx, ~.x %>% group_by(RACE2) %>% summarise(prop_withdrew = mean(cens_type==3),
                                                        N = n())) %>% 
  bind_rows(.id='index_condition') %>% 
  filter(!is.na(RACE2)) %>% 
  mutate(index_condition = transform_indx(index_condition)) %>% 
  mutate(prop_withdrew = round(prop_withdrew,3)) %>% 
  unite(Overall, c('prop_withdrew', 'N'), sep = ' / ')

out <- left_join(out1, out2, by=c("Index event" = 'index_condition','Race'='RACE2'))
openxlsx::write.xlsx(out, file='Withdrawal_age_race.xlsx')



# Median time after index condition to discontinuation ----------------------------------------

map(hosp_postdx_age, ~.x %>% 
      mutate(time_to_wd = as.numeric(surv_date - CLM_FROM)) %>% # time between dialysis and withdrawal
      group_by(agegrp_at_event, RACE2) %>% 
      summarise(median_time = median(time_to_wd, na.rm=T))) %>% 
  bind_rows(.id = 'index_condition') %>% 
  filter(!is.na(RACE2)) %>% 
  spread(agegrp_at_event, median_time) %>% 
  mutate(index_condition = transform_indx(index_condition)) %>% 
  rename('Index event' = 'index_condition', 'Race' = 'RACE2') %>% 
  openxlsx::write.xlsx(file = 'Time_to_withdrawal.xlsx')




# Cox regressions -----------------------------------------------------------------------------

## Cox regression from time of even to time of withdrawal, as a cause-specific hazard estimation
## problem

hosp_cox_data <- 
  map(hosp_postdx_age, ~.x %>% 
        left_join(select(Dat,  SEX, zscore, USRDS_ID)) %>% 
        mutate(time_from_event = as.numeric(surv_date-CLM_FROM)) %>% 
        mutate(time_on_dialysis = se_to_event_time) %>% 
        rename('Race' = "RACE2") %>% 
        filter(Race != 'Other') %>% 
        mutate(Race = droplevels(Race)))

# names(hosp_cox_data) <- c('Primary stroke','Stroke with complications',
#                           'Lung Cancer','Metastatic Cancer',
#                           'Dementia','Failure to thrive')
saveRDS(hosp_cox_data, file.path(dropdir, 'hosp_cox_data.rds'), compress = T)

## Kaplan Meier curves

fit_list = map(hosp_cox_data, ~survfit(Surv(time_from_event, cens_type==3)~ Race, 
                                       data = .))
cph1_list <-  map(hosp_cox_data, ~ coxph(Surv(time_from_event, cens_type==3)~ Race, 
                                         data = .))
logrank_list <- map(cph1_list, ~format.pval(anova(.)[2,4], eps=1e-6))
plt_list <- vector('list',6)
for(i in 1:6){
  plt_list[[i]] <- survMisc::autoplot(fit_list[[i]], type='single', censSize = 0,
                                      title = names(fit_list)[i],
                                      xLab = 'Days from index event',
                                      legTitle = '')$plot +
    scale_y_continuous('Percent who discontinued dialysis', labels = scales::percent)+
    annotate('text', x = 50, y = 0.1, label=paste0('p-value : ', logrank_list[[i]]),hjust=0) +
    theme(legend.position = 'bottom', legend.justification = c(0.5, 0.5))
}

pdf('KaplanMeierPlots.pdf')
for(i in 1:6) print(plt_list[[i]])
dev.off()

#'  Cox regressions adjusting for age at event, gender, race, z-score and time on dialysis at time of event
hosp_coxph <- map(hosp_cox_data,
                  ~ coxph(Surv(time_from_event, cens_type==3)~Race + agegrp_at_event + SEX + zscore + 
                            time_on_dialysis, data = .) %>% 
                    broom::tidy() %>% 
                    filter(str_detect(term, 'Race')) %>% 
                    select(term, estimate, p.value:conf.high) %>% 
                    mutate(term = str_remove(term, 'Race')) %>% 
                    mutate_at(vars(estimate, conf.low:conf.high), exp))
bind_rows(hosp_coxph, .id = 'index_event') %>% 
  ggplot(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high))+
    geom_pointrange() + 
    geom_hline(yintercept = 1, linetype =3)+
    facet_wrap(~index_event, nrow=2) +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    scale_y_continuous('HR for discontinuation, compared to Whites', breaks = seq(0.4,1.4, by = 0.2))+
    labs(x = '') +
    ggsave('ForestPlot.pdf')

bind_rows(hosp_coxph, .id = 'Index event') %>% 
  openxlsx::write.xlsx('CoxPH.xlsx')

# TODO: Figure out which analysis makes the most sense


# Evaluating how long from discontinuation to death -------------------------------------------

ProjTemplate::reload()
hospitalization <- readRDS('data/hospitalization_ids.rds')
Dat <- readRDS('data/rda/Analytic.rds')
Dat <- Dat %>% mutate(surv_date = pmin(cens_time, withdraw_time, DIED, TX1DATE, na.rm=T)) %>% 
  mutate(RACE2 = forcats::fct_relevel(RACE2, 'White'))

Dat <- Dat %>% 
  mutate(withdraw_to_death = ifelse(cens_type==3 & !is.na(BEGIN_withdraw),
                                    tod - tow, NA))
Dat %>% filter(RACE2 != 'Other') %>% ggplot(aes(x = RACE2, y = withdraw_to_death))+geom_boxplot()
Dat %>% filter(RACE2 != 'Other', cens_type==3) %>% 
  kruskal.test(withdraw_to_death~RACE2, data=.) %>% 
  broom::tidy()
Dat %>% filter(RACE2 != 'Other', cens_type == 3) %>% 
  ggplot(aes(x = withdraw_to_death*365.25))+
    geom_density(aes(group = RACE2, color = RACE2)) +
    xlim(0,100)
Dat %>% filter(RACE2 != 'Other', cens_type == 3) %>% 
  group_by(RACE2) %>% 
  summarise(med_surv = median(365*withdraw_to_death, na.rm=T))

# TODO: Sensitivity of all results to imputation of withdrawal time
# This means, we assumed, if withdrawal data is missing, that we used 7 days before death. 
# We should see how our results hold up if we impute the withdrawal date as 
# (a) the date of death
# (b) randomly drawn from race-specific distribution


# Assessing comorbidities from hospitalization ------------------------------------------------
## This is done in evaluate_comorbidities.R


# Matching comorbidity score with time of index condition -------------------------------------

index_condn_comorbs <- readRDS(file.path(dropdir, 'index_condn_comorbs.rds'))
hosp_post_dx <- readRDS(file.path(dropdir, 'final_hosp_data.rds'))

out <- list()
for (n in names(hosp_post_dx)){
  print(paste('Working on ', n))
  d <- index_condn_comorbs[[n]] %>% select(USRDS_ID:CLM_THRU, comorb_indx)
  hosp_post_dx[[n]] %>% mutate(CLM_FROM = as.character(CLM_FROM),
                               CLM_THRU = as.character(CLM_THRU)) %>% 
    left_join(d) %>% 
    group_by(USRDS_ID) %>% 
    filter(comorb_indx == max(comorb_indx)) %>% 
    ungroup() %>% 
    distinct() -> out[[n]]
}
assertthat::are_equal(map_int(hosp_post_dx, nrow), map_int(out, nrow))
hosp_post_dx <- out

# TODO: Model time to withdrawal as a function of age, sex, race, time to 
# index condition from FIRST_SE and comorbidity score, using parametric survival models
# 

Dat <- readRDS('data/rda/Analytic.rds')
hosp_cox_data <- readRDS(file.path(dropdir, 'hosp_cox_data.rds'))
modeling_data <- hosp_cox_data
for(n in names(modeling_data)){
  modeling_data[[n]] <- modeling_data[[n]] %>% left_join(out[[n]] %>% select(USRDS_ID, comorb_indx))
}

## Data munging to add simulated withdrawal times

modeling_data2 <- map(modeling_data, ~left_join(., select(Dat, USRDS_ID, toc:tow), by ='USRDS_ID') %>% 
                        mutate_at(vars(toc:tow), funs(.*365.25)) %>% 
                        split(.$Race)) # convert times to days


# Simulation study ----------------------------------------------------------------------------

conds <- names(modeling_data2)
cox_models <- list()
set.seed(10385)
for (cnd in conds) {
  D <- modeling_data2[[cnd]]
  weib <- survreg(Surv(time_from_event+0.1, cens_type==3)~ # Added 0.1 since weibull is > 0
                    agegrp_at_event + SEX  + time_on_dialysis +
                    zscore + comorb_indx, 
                  data = D$White, 
                  dist = 'weibull')
  D <- map(D,  ~mutate_at(., vars(toc:tow), funs(as.numeric(.) - as.numeric(time_on_dialysis))))
  shp <- 1/weib$scale
  sc <- map(D, ~exp(predict(weib, newdata = ., type = 'lp')))
  D$White <- D$White %>% mutate(new_tow = tow)
  
  cox_models[[cnd]] <- list()
  nsim <- 1000
  rw <- map(sc, ~matrix(rweibull(length(.)*nsim, shape = shp, scale = .), ncol = nsim, byrow = F))
  for (i in 1:nsim) {
    print(i)
    for(n in setdiff(names(D), 'White')){
      D[[n]]$new_tow <- rw[[n]][,i]
    }
    D <- map(D, ~mutate(., new_surv_time = pmin(toc, tod, tot, new_tow, na.rm=T)) %>% 
               mutate(new_cens_type = case_when(toc == new_surv_time ~ 0, 
                                                tod == new_surv_time ~ 1, 
                                                tot == new_surv_time ~ 2, 
                                                new_tow == new_surv_time ~ 3)) %>% 
               mutate(new_surv_time = ifelse(new_cens_type==3, new_surv_time + 7, new_surv_time))) # take withdrawal to death
    blah = bind_rows(D)
    cox_models[[cnd]][[i]] <- broom::tidy(coxph(Surv(new_surv_time, new_cens_type %in% c(1,3))~Race, data = blah))
  }
}
# stroke_primary = modeling_data2$stroke_primary
# 
# weib = survreg(Surv(time_from_event+0.1, cens_type==3)~ # Added 0.1 since weibull is > 0
#                  agegrp_at_event + SEX  + time_on_dialysis +
#                  zscore + comorb_indx, 
#                data = stroke_primary$White, 
#                dist = 'weibull')
# stroke_primary <- map(stroke_primary, ~mutate_at(., vars(toc:tow), funs(as.numeric(.) - as.numeric(time_on_dialysis))))
# shp = 1/weib$scale
# sc <- map(stroke_primary, ~exp(predict(weib, newdata = ., type = 'lp')))
# stroke_primary$White <- stroke_primary$White %>% 
#   mutate(new_tow = tow)
# 
# nsim <- 1000
# rw <- map(sc, ~matrix(rweibull(length(.)*nsim, shape = shp, scale = .), ncol=nsim, byrow=F))
# 
# cox_models <- list()
# # HRs <- matrix(rep(0,nsim*4), ncol = 4)
# for(i in 1:nsim){
#   print(i)
#   for(n in setdiff(names(stroke_primary), 'White')){
#     stroke_primary[[n]]$new_tow = rw[[n]][,i]
#   }
#   stroke_primary <- map(stroke_primary, ~mutate(., new_surv_time = pmin(toc, tod, tot, new_tow, na.rm=T)) %>% 
#                           mutate(new_cens_type = case_when(toc == new_surv_time ~ 0, tod == new_surv_time ~ 1, tot == new_surv_time ~ 2, new_tow == new_surv_time ~ 3)))
#   blah = bind_rows(stroke_primary)
#   cox_models[[i]] <- broom::tidy(coxph(Surv(new_surv_time, new_cens_type %in% c(1,3))~Race, data = blah))
#   # HRs[i,] <- coef(cph)
#   # sdiff <- survdiff(Surv(new_surv_time, new_cens_type %in% c(1,3))~Race, data = blah)
#   # pvals[i] <- pchisq(sdiff$chisq, 4, lower.tail = FALSE)
# }

bl <- map(cox_models, ~select(.,term, estimate) %>% spread(term, estimate)) %>% bind_rows() %>% 
  gather(Race, logHR) %>% 
  mutate(Race = str_remove(Race, 'Race'))

saveRDS(cox_models, file=file.path(dropdir,'sim_cox_stroke_p.rds'), compress = T)

# TODO: Do some residual analysis and validation of White Weibull model