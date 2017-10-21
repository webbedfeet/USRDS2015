# Script to interrogate IDs among those who discontinued dialysis to see if 
# the propensity to discontinue depends on race and prevalent comorbidities
# 
# We're not doing incident comorbidities today (10/19/17) since we're not sure 
# if prevalence at study entry is available in medevid. To me it appears not for most
# of the comorbidities
# 
# The IDs with prevalent comorbidities is interrogated in hospitalization.R and
# stored in data/hospitalization_idss.rds as a list by comorbidity.

source('lib/reload.R'); reload()
dbdir15 <- verifyPaths()
dbdir14 <- verifyPaths(year=2014)

con14 <- dbConnect(SQLite(), file.path(dbdir14, 'PR_db'))
dat <- tbl(con14, 'AnalyticData')
discontinue_id <- dat %>% filter(cens_type==3) %>% 
  select(USRDS_ID, RACE2, HispGrps) %>% collect() %>% 
  mutate(RACE2 = str_replace(RACE2, 'Nonhispanic ',''))

hosp_id <- readRDS(file.path(dbdir15,'hospitalization_ids.rds'))

disc_hosp_id <- lapply(hosp_id, filter, USRDS_ID %in% discontinue_id$USRDS_ID)
sapply(disc_hosp_id, nrow)
disc_hosp_race <- lapply(hosp_id, 
                         function(x) discontinue_id %>% 
                           filter(USRDS_ID %in% x$USRDS_ID) %>% count(RACE2))
