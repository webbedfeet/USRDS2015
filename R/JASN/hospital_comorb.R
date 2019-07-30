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
library(foreach)
library(parallel)
library(doParallel)
no_cores <- detectCores()-1

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


saveRDS(hosp_postdx_age, file.path(dropdir,'hosp_postdx_age.rds'), compress=T)

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


# Cox regressions: Data munging -----------------------------------------------------------------------------

## Cox regression from time of even to time of withdrawal, as a cause-specific hazard estimation
## problem

hosp_postdx_age <- readRDS(file.path(dropdir, 'hosp_postdx_age.rds'))
hosp_cox_data <-
  map(hosp_postdx_age, ~.x %>%
        left_join(select(Dat,  SEX, zscore, USRDS_ID, REGION, toc:tow)) %>%
        mutate(time_from_event = as.numeric(surv_date-CLM_FROM)) %>%
        mutate(time_on_dialysis = se_to_event_time) %>%
        rename('Race' = "RACE2") %>%
        filter(Race != 'Other') %>%
        mutate(Race = droplevels(Race)))

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

modeling_data <- hosp_cox_data
for(n in names(modeling_data)){
  modeling_data[[n]] <- modeling_data[[n]] %>% left_join(out[[n]] %>% select(USRDS_ID, comorb_indx))
}

## Data munging to add simulated withdrawal times

Dat <- readRDS('data/rda/Analytic.rds')
modeling_data2 <- map(modeling_data, ~left_join(., select(Dat, USRDS_ID, REGION, toc:tow), by ='USRDS_ID') %>%
                        mutate_at(vars(toc:tow), funs(.*365.25)) %>%
                        mutate(time_on_dialysis = as.numeric(time_on_dialysis),
                               REGION = as.factor(REGION),
                               agegrp_at_event = fct_collapse(agegrp_at_event,
                                                              '<50' = c('<40','[40,50)'))) %>% 
                        split(.$Race)) # convert times to days

save(modeling_data, modeling_data2, file = file.path(dropdir,'modeling_data.rda'), compress=T)
