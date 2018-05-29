# Creating an analytic dataset from 2015 data


# Setup ---------------------------------------------------------------------------------------

ProjTemplate::reload()
dbdir <- verifyPaths(); dir.exists(dbdir)
dropdir <- file.path(ProjTemplate::find_dropbox(), 'NIAMS','Ward','USRDS2015','data')


# Extract from DB -----------------------------------------------------------------------------


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

Dat <- Dat %>% 
  mutate(dmfail = ifelse(str_detect(PDIS,'^250'), 'Y','N'),
         DIABETES = ifelse(dmfail=='Y', 'Y','N'),
         DIABETES = ifelse(DIABETES=='N' & (DIABINS=='Y' | DIABPRIM=='Y'), 'Y', DIABETES)) %>% 
  select(-dmfail, -DIABINS, -DIABPRIM)
saveRDS(Dat, file = 'data/rda/rawdata.rds', compress = TRUE)

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
#non_uniques %>% select(-USRDS_ID) %>% summarise_all(funs(sum(. > 1, na.rm=T))) %>% View()

#' BMI is missing in most, and does change with exam, possibly. We will first replace all available BMI recordings with their 
#' median BMI over observations. We will also normalize the dichotomous variables to keep the modal value over the 
#' observations 

# Dat_id <- Dat %>% nest(-USRDS_ID)
Dat_grp <- Dat %>% group_by(USRDS_ID)

# Fixing BMI
Dat_grp <- Dat_grp %>% mutate(BMI = median(BMI, na.rm = T)) %>% distinct()
## 2,783,664 obs, 2,675,390 uniques


# Normalzing dichotomous variables

Dat_grp <- Dat_grp %>% 
  mutate_at(vars(Cancer:Smoke), normalize_dichot) %>% 
  distinct()
counts(Dat)
# 2,734,641 obs, 2,675,390 unique

# Normalize ethnicity
Dat_grp <- Dat_grp %>% mutate(ETHN = ifelse(ETHN == '4', NA, ETHN)) %>% 
  #group_by(USRDS_ID) %>% 
  mutate(ETHN2 = normalize_ethn(ETHN)) %>% 
  #ungroup() %>% 
  mutate(ETHN = ETHN2) %>% 
  select(-ETHN2) %>% 
  mutate(ETHN = ifelse(is.na(ETHN), '4', ETHN)) %>% 
  distinct()
# 2,697,112 obs, 2,675,390 unique

# Normalize diabetes

Dat_grp <- Dat_grp %>% 
  mutate(DIABETES  = normalize_dm(DIABETES)) %>% 
  distinct()
saveRDS(Dat_grp,file = 'data/rda/tmp.rds',compress=T)
# 2,668,050 obs, 2,675,390 unique

# Normalize country
# Stuck here now
Dat_grp <- Dat_grp %>% 
  mutate(COUNTRY = normalize_country(COUNTRY)) %>% 
  distinct()
saveRDS(Dat,file = 'data/rda/tmp.rds',compress=T)

# 2,675,392 obs, 2,675,390 unique

#' Individual 732813 has affiliation to both Mexico and PR. I will put this individual in with PR. This is the only 
#' discrepancy left

Dat <- Dat_grp %>% ungroup()

Dat[Dat$USRDS_ID == 732813,"COUNTRY"] <-  174
Dat <- distinct(Dat)
# 2,675,390 obs, 2,675,390 unique


# Update regions
a1 <- as.character(c(23, 50, 33, 25, 09, 36, 42, 44, 34)) # Northeast
a2 <- as.character(c(48, 40, 05, 22, 28, 01, 47, 21, 12, 13, 45, 37, 51, 11, 24, 10, 54,78, 72 )) # South
a3 <- as.character(c(20, 31, 46, 38, 29, 19, 27, 17, 55, 18, 26, 39)) # Midwest
a4 <- as.character(c(02, 15, 06, 41, 53, 32, 04, 49, 16, 35, 08, 56, 30, 66, 69, 60,64)) # West

Dat <- Dat %>%
  mutate(REGION = case_when(
    Dat$STATE %in% a1 ~ "Northeast",
    Dat$STATE %in% a2 ~ 'South',
    Dat$STATE %in% a3 ~ "Midwest",
    Dat$STATE %in% a4 ~ "West"
  ))

saveRDS(Dat, file = 'data/rda/normalizedData.rds')


# Missing value imputation ------------------------------------------------

#Dat %>% summarise_all(funs(100*mean(is.na(.)))) %>% View()

#' Missing data:
#' + BMI: 23.47% (2014 data had 0%)
#' + COUNTRY: 92.65% (2014 data had 88.14% )
#' We might fill in some gaps with 2014 data
#' We need to impute data on comorbidities using kNN

d <- Dat %>% select(Cancer:Smoke) %>% 
  mutate_all(funs(ifelse(.=='Y', 1,0))) %>% 
  as.data.frame()
dpr <- t(as.matrix(d))
ind <- which(apply(dpr, 2, function(x) sum(is.na(x)))==8)
n <- ncol(dpr); p <- nrow(dpr)
d1pr <- impute.knn(dpr[,-ind], colmax=0.9)
bl <- matrix(NA, p, n)
bl[,-ind] <- d1pr$data
d1 <- data.frame(t(round(bl)))
d1 <- d1 %>% mutate_all(funs(factor(ifelse(.==1,'Y','N'))))
v <- row.names(d1pr$data)
Dat[,v] <- d1

saveRDS(Dat,'data/rda/imputedData.rds')

# Filtering based on values -----------------------------------------------

## First_SE after 1/1/2003

Dat <- Dat %>% filter(FIRST_SE >= '2003-01-01') # 1,382,049 observations

# First_SE before 12/31/2014, so at least 6 months followup for all

Dat <- Dat %>% filter(FIRST_SE <= '2014-12-31') # 1,329,966 observations

## Transplant after first exam
ind <- with(Dat, FIRST_SE==TX1DATE)
ids <- Dat$USRDS_ID[which(ind & !is.na(ind))]
`%notin%` <- Negate('%in%')
Dat <- Dat %>% filter(USRDS_ID %notin% ids) %>% 
  filter(INC_AGE >= 18 & INC_AGE <= 100) %>% 
  filter(!is.na(SEX)) %>% 
  mutate(His.Nhis = ifelse(HISPANIC==1, ifelse(!is.na(COUNTRY),2,1),2)) 

# 1,291,001 obs, 1,291,001 uniques

Dat <- Dat %>%   select(USRDS_ID, FIRST_SE, PDIS, RXSTOP, TX1DATE, USA, ZIPCODE, DIALTYPE, DIABETES, DIED, SEX,
         REGION, BMI, COUNTRY, STATE, INC_AGE, RACE, HISPANIC, Cancer:Smoke, His.Nhis)



# Data munging ------------------------------------------------------------

Dat <- Dat %>% mutate(RACE = ifelse(RACE=='9','5', RACE)) %>% 
  mutate(RACE2 = case_when(
    RACE == '1' ~ 'Native American',
    RACE == '2' ~ 'Asian',
    RACE == '3' ~ 'Black',
    RACE =='4' ~ 'White',
    RACE == '5' ~ 'Other')) %>% 
  mutate(RACE2 = ifelse(HISPANIC==1, 'Hispanic', RACE2))

Dat <- Dat %>% mutate(AGEGRP = cut(INC_AGE, c(18, 29,39,49,59,69,100), include.lowest = T))

# Change country code > 240 to missing
Dat <- Dat %>% mutate(COUNTRY = ifelse(COUNTRY > 240, NA, COUNTRY))

# Update PR status

PRfolks <- Dat %>% filter(HISPANIC==1 & (COUNTRY == 174 | STATE == 72)) %>% dplyr::pull(USRDS_ID)
Dat <- Dat %>% mutate(COUNTRY = ifelse(USRDS_ID %in% PRfolks, 174, COUNTRY))


# Indicator for hispanics

Dat <- Dat %>% mutate(HispSubgrp = ifelse(HISPANIC == 1 | RACE2 =='White', 1, 0))
Dat <- Dat %>% 
  mutate(HispPR = ifelse(HISPANIC==1 & COUNTRY == 174 & !is.na(COUNTRY), 1,0),
         HispUS = ifelse(HISPANIC == 1 & (COUNTRY %in% c(227,88,235,160,5) & !is.na(COUNTRY)), 2,0),
         HispNon = ifelse(HISPANIC == 1 & HispPR==0 & HispUS == 0 & !is.na(COUNTRY), 3, 0),
         HispUnk = ifelse (HISPANIC == 1 & is.na(COUNTRY), 4,0)) %>% 
  mutate(HispGrps = HispPR + HispUS + HispNon + HispUnk) %>% 
  mutate(HispGrps = ifelse(HispSubgrp == 1, HispGrps, NA)) %>% 
  mutate(HispGrps = factor(HispGrps, labels = c('Nonhispanic White','HispPR','HispUS','HispOther', 'HispUnknown')))

saveRDS(Dat, 'data/rda/interim.rds')


# Defining withdrawal from rxhist -------------------------------------------------------------

rxhist <- tbl(sql_conn, 'rxhist60') %>% collect(n = Inf) %>% filter(USRDS_ID %in% Dat$USRDS_ID)

txpattern <- rxhist %>% group_by(USRDS_ID) %>% 
  summarise(tx = paste(RXGROUP, collapse='')) %>% # Create string of treatment pattern
  ungroup()

ids <- Dat %>% filter(RXSTOP %in% c('A','C','D','E')) %>% dplyr::pull(USRDS_ID)
txpattern_withdraw <- txpattern %>%
  filter(str_detect(tx, 'B')) %>% # Discontinued
  filter(!str_detect(str_match(tx, '.+[B](.*)')[,2],'[0-9]')) # but no further dialysis

blah <- rxhist %>% semi_join(txpattern_withdraw, by='USRDS_ID') %>% 
  filter(RXGROUP=='B') %>% 
  group_by(USRDS_ID) %>% 
  filter(BEGDATE == max(BEGDATE)) %>% 
  ungroup() %>% 
  mutate(BEGIN_withdraw = BEGDATE) %>% 
  select(USRDS_ID, BEGIN_withdraw) %>% 
  filter(USRDS_ID %in% ids)

Dat <- Dat %>% left_join(blah)

saveRDS(Dat, file = 'data/rda/interim2.rds', compress = T)

# Loss to follow-up and/or recovery of function -----------------------------------------------

txpattern_cens <- txpattern %>% filter(str_detect(tx, 'X|Z')) %>% 
  filter(!str_detect(str_match(tx, '.+[X|Z](.*)')[,2], '[0-9]'))
# no dialysis after loss to followup (X) or recovery of function (Z)

crud_x <- rxhist %>% 
  filter(USRDS_ID %in% txpattern_cens$USRDS_ID) %>% 
  filter(RXGROUP == 'X') %>% 
  group_by(USRDS_ID) %>% 
  filter(BEGDATE == max(BEGDATE)) %>% 
  ungroup()
crud_z <- rxhist %>% 
  filter(USRDS_ID %in% txpattern_cens$USRDS_ID) %>% 
  filter(RXGROUP == 'Z') %>% 
  group_by(USRDS_ID) %>% 
  filter(BEGDATE == max(BEGDATE)) %>% 
  ungroup()

crud = rbind(crud_x, crud_z) %>% 
  group_by(USRDS_ID) %>% 
  filter(BEGDATE == min(BEGDATE)) %>%  # find earlier of X and Z dates
  ungroup() %>% 
  mutate(BEGIN_cens = BEGDATE) %>% 
  select(USRDS_ID, BEGIN_cens)

Dat <- Dat %>% left_join(crud) %>% 
  mutate(withdraw = ifelse(RXSTOP %in% c('A','C','D','E'),1,0),
         withdraw_time = case_when(
           withdraw==1 & is.na(BEGIN_withdraw) ~ as.character(as.Date(DIED) - 7),
           withdraw == 1 & !is.na(BEGIN_withdraw) ~ BEGIN_withdraw,
           withdraw==0 ~ NA_character_),
          cens_time = pmin(BEGIN_cens, '2015-01-01', na.rm=T))
Dat <-  Dat %>% 
  mutate(cens_time = ifelse(cens_time < FIRST_SE, NA, cens_time),
         withdraw_time = ifelse(withdraw_time < FIRST_SE, NA, withdraw_time))


# Compute survival times --------------------------------------------------

Dat <- Dat %>% 
  mutate_at(vars(DIED, FIRST_SE, TX1DATE, cens_time, withdraw_time), as.Date) %>% 
  mutate(
  toc = (cens_time - FIRST_SE)/365.25, # Censoring
  tod = (DIED - FIRST_SE)/365.25, # Death
  tot = (TX1DATE - FIRST_SE)/365.25,  # Transplant
  tow = (withdraw_time - FIRST_SE)/365.25) %>% # Withdrawal
  # select(USRDS_ID, toc, tod, tot, tow) %>% 
  mutate_at(vars(toc:tow), function(x) ifelse(x < 0, NA, x)) %>% 
  mutate(surv_time = pmin(toc, tod, tot, tow, na.rm=T)) %>% 
  mutate(cens_type = case_when(toc == surv_time ~ 0, tod == surv_time ~ 1, tot == surv_time ~ 2, tow == surv_time ~ 3))


# Add zscore --------------------------------------------------------------

zipses <- haven::read_sas(file.path(dbdir,'2015 Core', 'core','zipses1.sas7bdat'))
Dat <- Dat %>% left_join(zipses, by=c('ZIPCODE' = 'zipcode'))

saveRDS(Dat, file = 'data/rda/Analytic.rds', compress = T)
saveRDS(Dat, file = file.path(dropdir, 'Analytic.rds'), compress=T)
# saveRDS(Dat, file = file.path(ProjTemplate::find_dropbox(),'NIAMS','Ward','USRDS2915','data','Analytic.rds'))


# Save into SQLite --------------------------------------------------------

dbWriteTable(sql_conn, 'zipses', zipses, overwrite = T)
dbWriteTable(sql_conn, 'AnalyticData', Dat, overwrite = T)

studyids <- Dat %>% select(USRDS_ID)
dbWriteTable(sql_conn, 'StudyIDs', studyids, overwrite = TRUE)

dbDisconnect(sql_conn)
