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


ProjTemplate::reload()
dbdir = verifyPaths()
dropdir <- file.path(ProjTemplate::find_dropbox(), 'NIAMS','Ward','USRDS2015','data')
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



# Propensity of withdrawal based on timing of Comorb ------------------------------------------

#' what's the chance of discontinuation, by race

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
    # mutate(agegrp_at_event = factor(agegrp_at_event,
                         # levels = c('<40','[40,50)','[50,60)','[60,70)','[70,80)','80+'))) %>% )

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
  mutate(index_condition = tranform_indx(index_condition)) %>% 
  mutate(prop_withdrew = round(prop_withdrew,3)) %>% 
  unite(Overall, c('prop_withdrew', 'N'), sep = ' / ')

out <- left_join(out1, out2, by=c("Index event" = 'index_condition','Race'='RACE2'))
openxlsx::write.xlsx(out, file='Withdrawal_age_race.xlsx')



# Median time after index condition to discontinuation ----------------------------------------

map(hosp_postdx_age, ~.x %>% 
      mutate(time_to_wd = as.numeric(surv_date - CLM_FROM)) %>% 
      group_by(agegrp_at_event, RACE2) %>% 
      summarise(median_time = median(time_to_wd, na.rm=T))) %>% 
  bind_rows(.id = 'index_condition') %>% 
  filter(!is.na(RACE2)) %>% 
  spread(agegrp_at_event, median_time) %>% 
  mutate(index_condition = transform_indx(index_condition)) %>% 
  rename('Index event' = 'index_condition', 'Race' = 'RACE2') %>% 
  openxlsx::write.xlsx(file = 'Time_to_withdrawal.xlsx')




# Cox regressions -----------------------------------------------------------------------------


mods1 <- map(hosp_last, ~Dat %>% semi_join(.x) %>% 
               filter(cens_type %in% c(0,3)) %>%  # Avoid cause-specific hazard by filtering
              coxph(Surv(surv_time, cens_type==3) ~AGEGRP + SEX + RACE2 + rcs(zscore), data=.))
res1 <- map(mods1, ~ confint(.) %>% exp() %>% as.data.frame() %>% rownames_to_column('variable') %>% 
              filter(str_detect(variable, 'RACE2')) %>% mutate(variable = str_remove(variable, "RACE2")))
res2 <- map(mods1, ~summary(.x)$coef %>% as.data.frame() %>% rownames_to_column('variable') %>% 
              filter(str_detect(variable, 'RACE2')) %>% select(variable,`exp(coef)`) %>% 
              mutate(variable = str_remove(variable, 'RACE2')) %>% 
              rename('Estimate' = `exp(coef)`))
results = map2(res2, res1, left_join, by='variable')

# TODO: Count time from last comorbidity date
# TODO: Figure out which analysis makes the most sense

# Verifying incident cases from medevid ------------------------------------

## CVATIA, CVA = stroke
## CANC = cancer

sql_conn <- dbConnect(SQLite(), file.path(dbdir, 'USRDS.sqlite3'))
medevid <- tbl(sql_conn, 'medevid')
studyids <- tbl(sql_conn, 'StudyIDs')
medevid %>% inner_join(studyids) %>% count()
joined_tbl <- medevid %>% inner_join(studyids)

bl <- medevid %>% inner_join(studyids) %>% collect(n=5) # Exploration

## CANCER and COMO_CANC are identical as are CVA and COMO_CVATIA. So you need only one
bl2 <- joined_tbl %>% select(USRDS_ID, CVA,  CANCER,  CTDATE, DIALDAT, DIALEDT, DIED) %>%
  collect(n=Inf) %>%
  filter(CVA != '') # Same individuals are missing CVA and CANCER
bl2 <- bl2 %>%
  group_by(USRDS_ID) %>%
  top_n(-1, DIALDAT) %>% # Take the earliest dialysis date
  ungroup()
