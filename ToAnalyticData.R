# Creating an analytic dataset from 2015 data
ProjTemplate::reload()
dbdir <- verifyPaths()
sql_conn <- dbConnect(SQLite(),file.path(dbdir,'USRDS.sqlite3'))

P <- tbl(sql_conn, 'patients') %>% 
  select(BORN, DIALTYP, DIED, DISGRPC, FIRST_SE, FIRSTDIAL, HISPANIC, INC_AGE, INCYEAR, NETWORK, RACE, PDIS, RXSTOP, SEX, STATE, TOTTX, TX1DATE, USA, USRDS_ID, ZIPCODE) %>% 
  mutate(
    DIALTYPE = if(DIALTYP=='' | DIALTYP == 'A') 3 else {
      if (DIALTYP < 4) 1 else 2},
    RACE2 = if (RACE==9) 5 else RACE) %>% 
  mutate(RACE = RACE2) %>% select(-RACE2, -DIALTYP)
# 2,830,454 unique individuals


M <- tbl(sql_conn, 'medevid') %>% 
  select(ALBUM, BMI, CANCER, CARFAIL, COMO_ASHD, COMO_CANC, COMO_CHF, COMO_COPD, COMO_CVATIA, COMO_HTN,
         COMO_PVD, COMO_TOBAC, COUNTRY, CVA, DIABINS, DIABPRIM, DIALDAT, DIALEDT, DIALTYP, DIED, DISGRPC, DYSRHYT, ETHN,
          HIV, HYPER, IHD, INC_AGE, NETWORK, PULMON, PVASC, RACE, SEX, SMOKE, TDATE, USRDS_ID, RACE) %>% 
  mutate(Cancer = if (CANCER=='Y' | COMO_CANC == 'Y') 'Y' else 'N',
         Cardia = if (CARFAIL == 'Y'|COMO_CHF == 'Y') 'Y' else 'N', 
         Cva = if (CVA == 'Y' | COMO_CVATIA == 'Y') "Y" else "N",
         Hyper = if (HYPER == "Y" | COMO_HTN == "Y") "Y" else "N",
         Ihd = if (IHD == "Y" | COMO_ASHD == "Y") "Y" else "N",
         Pulmon = if (PULMON == "Y" | COMO_COPD == "Y") "Y" else "N",
         Pvasc = if (PVASC == "Y"  | COMO_PVD == "Y") "Y" else "N",
         Smoke = if (SMOKE == "Y" | COMO_TOBAC == "Y") "Y" else "N"
  ) %>% 
  select(BMI, COUNTRY, ETHN, USRDS_ID , DIABINS, DIABPRIM, Cancer, Cardia, Cva, Hyper, Ihd, Pulmon, Pvasc, Smoke) 
# 2,812,913, with 2,675,390 unique observations

Dat <- inner_join(P, M) %>% collect(n=Inf) %>% distinct()
# 2,802,269, with 2,675,390 unique observations
saveRDS(Dat, file = 'data/rda/rawdata.rds')

# Check why there are duplicates ------------------------------------------

char_na <- function(x){
  return(ifelse(x == '', NA, x))
}
Dat <- Dat %>% mutate_if(is.character, char_na)
multiple_row_ids <- Dat %>% count(USRDS_ID) %>% 
  filter(n > 1) %>% 
  dplyr::pull(USRDS_ID)
non_uniques <- Dat %>% filter(USRDS_ID %in% multiple_row_ids) %>% 
  group_by(USRDS_ID) %>% summarise_all(funs(length(unique(.)))) %>% ungroup()
non_uniques %>% select(-USRDS_ID) %>% summarise_all(funs(sum(. > 1, na.rm=T))) %>% View()

#' BMI is missing in most, and does change with exam, possibly. We will first replace all available BMI recordings with their 
#' median BMI over observations. We will also normalize the dichotomous variables to keep the modal value over the 
#' observations 

# Fixing BMI
Dat <- Dat %>% group_by(USRDS_ID) %>% 
  mutate(BMI2 = median(BMI, na.rm=T)) %>% ungroup() %>% 
  mutate(BMI = BMI2) %>% 
  select(-BMI2) %>% 
  distinct()
# 2,786,144 obs, 2,675,390 uniques

# Normalzing dichotomous variables

Dat <- Dat %>% group_by(USRDS_ID) %>% 
  mutate_at(vars(DIABINS:Smoke), normalize_dichot) %>% 
  ungroup() %>% 
  distinct()
# 2,729,349 obs, 2,675,390 unique

# Normalize ethnicity
Dat <- Dat %>% mutate(ETHN = ifelse(ETHN == '4', NA, ETHN)) %>% 
  group_by(USRDS_ID) %>% 
  mutate(ETHN2 = normalize_ethn(ETHN)) %>% 
  ungroup() %>% 
  mutate(ETHN = ETHN2) %>% 
  select(-ETHN2) %>% 
  mutate(ETHN = ifelse(is.na(ETHN), '4', ETHN)) %>% 
  distinct()
# 2,688,050 obs, 2,675,390 unique

# Normalize country

Dat <- Dat %>% group_by(USRDS_ID) %>% 
  mutate(COUNTRY = normalize_country(COUNTRY)) %>% 
  ungroup() %>% 
  distinct()
# 2,675,392 obs, 2,675,390 unique

#' Individual 732813 has affiliation to both Mexico and PR. I will put this individual in with PR. This is the only 
#' discrepancy left

Dat[Dat$USRDS_ID==732813,"COUNTRY"] <-  174
Dat <- distinct(Dat)
# 2,675,390 obs, 2,675,390 unique

saveRDS(Dat, file = 'data/rda/normalizedData.rds')
