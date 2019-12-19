# Code for getting white and rest analytic datasets for simulation study

# Setup -------------------------------------------------------------------

abhiR::reload()

dropdir <- path(find_dropbox(), 'NIAMS','Ward','USRDS2015','data')
analytic_data <- read_fst(path(dropdir,'Analytic.fst'))

dbdir <- verifyPaths()
# sql_conn <- dbConnect(SQLite(), file.path(dbdir, 'USRDS.sqlite3'))

## Following paths are for MBP
sql_conn <- dbConnect(SQLite(), file.path('data','raw','USRDS.sqlite3'))

till2009 <- tbl(sql_conn, 'till2009')
from2010 <- tbl(sql_conn,'from2010')
studyids <- tbl(sql_conn, 'StudyIDs')
analytic <- tbl(sql_conn, 'AnalyticData')

# "create table hosp1 as select * from till2009 where USRDS_ID in (select USRDS_ID from Whites)"
# "create table hosp2 as select * from from2010 where USRDS_ID in (select USRDS_ID from Whites)"

# hosp1 <- tbl(sql_conn, 'hosp1')
# hosp2 <- tbl(sql_conn, 'hosp2')
# Fixing the REGION specification -----------------------------------------

a1 <- str_pad(as.character(c(23, 50, 33, 25, 09, 36, 42, 44, 34)), 2, pad='0') # Northeast
a2 <- str_pad(as.character(c(48, 40, 05, 22, 28, 01, 47, 21, 12, 13, 45, 37, 51, 11, 24, 10, 54,78, 72 )), 2, pad='0') # South
a3 <- str_pad(as.character(c(20, 31, 46, 38, 29, 19, 27, 17, 55, 18, 26, 39)), 2, pad = '0') # Midwest
a4 <- str_pad(as.character(c(02, 15, 06, 41, 53, 32, 04, 49, 16, 35, 08, 56, 30, 66, 69, 60,64)), 2, pad='0') # West

analytic_data <- analytic_data %>%
  mutate(REGION = case_when(
    analytic_data$STATE %in% a1 ~ "Northeast",
    analytic_data$STATE %in% a2 ~ 'South',
    analytic_data$STATE %in% a3 ~ "Midwest",
    analytic_data$STATE %in% a4 ~ "West"
  ))



# Adding DISGRPC codes to analytic data --------------------------------------------

disgrpc_code <- readRDS(path(dropdir, 'disgrpc_code.rds')) # See cause_esrd.R
disgrpc <- fst(path(dropdir, 'raw_data.fst'))[,c('USRDS_ID','DISGRPC')] %>%
  distinct() %>%
  left_join(disgrpc_code, by=c('DISGRPC'='Format')) %>%
  mutate(Description = ifelse(Description %in% c('8','**OTHER**'), NA, Description)) %>%
  mutate(Description = fct_other(Description,keep=c('Diabetes','Hypertension','Glomeruloneph.'))) %>%
  rename(ESRD_Cause = Description) 
analytic_data <- analytic_data %>% left_join(disgrpc) # Adding reason for dialysis


# Adding comorbidities ------------------------------------------------
## We wil add albumin, alcohol use and drug use to the set of comorbidities,
## and we'll utilize BMI and Smoking which are already included

medevid <- studyids %>% left_join(tbl(sql_conn, 'medevid')) %>% 
  select(USRDS_ID, ALBUM, ALBUMDT, ALBUMLM,ALCOH, DRUG) %>% 
  collect(n = Inf) %>% 
  mutate_at(vars(ALCOH, DRUG), ~ifelse(. == '', NA, .)) %>% distinct()
medevid1 <- medevid %>% left_join(analytic_data %>% select(USRDS_ID, FIRST_SE)) %>% 
  filter_at(vars(ALCOH:DRUG), ~!is.na(.)) %>% 
  mutate(ALBUMDT = as.Date(ALBUMDT),
         time_from_se = abs(FIRST_SE - ALBUMDT))
bl <- filter(medevid1, !is.na(ALBUM))
bl %>% count(USRDS_ID) %>% filter(n > 1) %>% left_join(bl) -> bl2 # Individuals with more than one record
bl2 %>% 
  mutate_at(vars(ALCOH, DRUG), ~ifelse(.=='Y', 1, 0)) %>% # Transform to numeric
  group_by(USRDS_ID) %>% 
  filter(time_from_se == min(time_from_se, na.rm=T)) %>% # Keep the earliest record
  mutate(ALBUM = mean(ALBUM, na.rm=T)) %>% # If you had repeats, average Albumin
  mutate_at(vars(ALCOH, DRUG), ~max(., na.rm=T)) %>% # take max of alcohol and drug
  ungroup() %>% 
  select(-ALBUMLM, -ALBUMDT) %>%
  distinct() %>% 
  assertr::verify(nrow(.)==length(unique(USRDS_ID))) -> bl3


medevid1 <- medevid1 %>% 
  count(USRDS_ID) %>% 
  filter(n==1) %>% 
  left_join(medevid1) %>% 
  select(-ALBUMDT, -ALBUMLM) %>% 
  mutate_at(vars(ALCOH, DRUG), ~ifelse(.=='Y',1,0)) %>% bind_rows(bl3) %>% 
  select(-n, -time_from_se)

blah <- analytic_data %>% left_join(medevid1 %>% select(-FIRST_SE))
blah %>% group_by(lubridate::year(FIRST_SE)) %>% 
  summarize_at(vars(Cancer:Smoke, ALBUM, ALCOH, DRUG), ~mean(is.na(.))*100)
blah %>% summarize_at(vars(Cancer:Smoke, ALBUM, ALCOH, DRUG), ~sum(is.na(.)))


analytic_data <- analytic_data %>% left_join(medevid1 %>% select(-FIRST_SE))

analytic_data <- analytic_data %>% 
  mutate(cens_type2 = case_when(
    cens_type == 0 ~ "Lost to followup",
    cens_type == 1 ~ "Dead",
    cens_type == 2 ~ "Transplant",
    cens_type == 3 ~ "Discontinued",
    TRUE ~ NA_character_)) # NA of type character


# Categorizing BMI --------------------------------------------------------

analytic_data <- analytic_data %>% 
  mutate(BMI2 = case_when(
    BMI < 18.5 ~ "Underweight",
    BMI >= 18.5 & BMI < 25 ~ "Normal",
    BMI >= 25 & BMI < 30 ~ "Overweight",
    BMI >= 30 ~ "Obese",
    TRUE ~ NA_character_
  )) %>% 
  mutate(BMI2 = as.factor(BMI2))


# Ensuring factors are factors --------------------------------------------

analytic_data <- 
  analytic_data %>% 
  mutate(DRUG = as.factor(DRUG),
         ALCOH = as.factor(ALCOH))

analytic_dt <- as.data.table(analytic_data)
##%######################################################%##
#                                                          #
####         Extracting baseline clinical data          ####
#                                                          #
##%######################################################%##

abhiR::reload()
dropdir <- path(find_dropbox(), 'NIAMS','Ward','USRDS2015','data')


## We want to compute the comorb_indx at the time of start of dialysis
## i.e., at FIRST_SE
## Mapping from existing variables to comorb_indx components
## Components of comorb_indx
##
## ASHD = IHD | COMO_ASHD = Ihd
## CHF = CARFAIL | COMO_CHF = Cardia
## CVATIA = CARFAIL | COMO_CVATIA = Cva
## PVD = PVASC | COMO_PVD = Pvasc
## Other.Cardiac = COMO_OTHCARD
## COPD = PULMON | COMO_COPD = Pulmon
## GI.Bleeding
## Liver
## Dysrhhythmia = DYSRHYT
## Cancer = CANCER | COMO_CANC = Cancer
## Diabetes = DIABETESÒ

## Extract COMO_OTHCARD & DYSRYTH from medevid and sync with analytic
## We'll extract GI.Bleeding and Liver disease from claims data using a 6 month 
## (180 day) window around the data FIRST_SE, following Liu, et al. 
## For any of the comorbidities that have missing values, we'll impute a 0 (not present)

sql_conn <- dbConnect(SQLite(), 'data/raw/USRDS.sqlite3')
new_comorbs <- dbGetQuery(sql_conn,
                          "select USRDS_ID, COMO_OTHCARD, DYSRHYT from medevid")
sid <- dbReadTable(sql_conn, 'StudyIDs')
dbDisconnect(sql_conn)

new_comorbs <- semi_join(new_comorbs, sid) # Restrict to analytic subjects

### Normalize to remove duplicate ids
setDT(new_comorbs)
bl1 <- new_comorbs[,.N,by=USRDS_ID][N==1][new_comorbs, on="USRDS_ID", nomatch=0]
bl1[,N := NULL]
bl1[bl1==''] <- NA
bl2 <- new_comorbs[, .N, by=USRDS_ID][N>1][new_comorbs, on="USRDS_ID", nomatch =0]
bl2[,N := NULL]
bl2[,':='(COMO_OTHCARD=normalize_dichot(COMO_OTHCARD),
          DYSRHYT=normalize_dichot(DYSRHYT)),
    by = USRDS_ID]
bl2 <- unique(bl2)
new_como <- rbind(bl1, bl2)

# analytic_dt <- read_fst(path(dropdir, 'Analytic.fst'), as.data.table = T)
analytic_dt <- merge(analytic_dt, new_como, by = 'USRDS_ID', all.x=T)

## Extract GI.Bleeds and Liver disease from clinical data

comorb_codes <- list(
  'ASHD' = '410-414, V4581, V4582',
  'CHF' = '39891, 422, 425, 428, 402X1,404X1, 404X3, V421',
  'CVATIA' = '430-438',
  'PVD' = '440-444, 447, 451-453, 557',
  'Other cardiac' = '420-421, 423-424, 429, 7850-7853,V422,V433',
  'COPD' = '491-494, 496, 510',
  'GI Bleeding' = '4560-4562, 5307, 531-534, 56984, 56985,578',
  'Liver' = '570-571,5721, 5724,5731-5733,V427',
  'Dysrhhythmia' = '426-427,V450, V533',
  'Cancer' = '140-172, 174-208, 230-231, 233-234',
  'Diabetes' = '250, 3572, 3620X, 36641'
) %>% map(icd9_codes)


### I'm reading, then left-joining with study ids, then removing rows with missing values
till2009 <- read_fst('data/raw/till2009.fst', as.data.table=T)[sid, on='USRDS_ID'][!is.na(CLM_FROM)]
from2010 <- read_fst('data/raw/from2010.fst', as.data.table=T)[sid, on='USRDS_ID'][!is.na(CLM_FROM)]
dx_date <- analytic_dt[,c('USRDS_ID','FIRST_SE'), with=FALSE]

### Merging to get clinical dates and FIRST_SE together
dat_dx1 <- merge(till2009[,c("USRDS_ID",'CLM_FROM','CLM_THRU')], dx_date, all.x=T, by='USRDS_ID')
dat_dx2 <- merge(from2010[,c('USRDS_ID','CLM_FROM','CLM_THRU')], dx_date, all.x=T, by='USRDS_ID')
dat_dx <- rbind(dat_dx1, dat_dx2)
dat_dx[,':='(CLM_FROM=as.Date(CLM_FROM), CLM_THRU=as.Date(CLM_THRU))]
setkey(dat_dx, 'USRDS_ID','CLM_FROM')
dat_dx[, ':='(tt_from = abs(FIRST_SE - CLM_FROM), # Find time from FIRST_SE to clinical visit
              tt_thru = abs(FIRST_SE - CLM_THRU))][
                ,min_days := as.numeric(pmin(tt_from, tt_thru))
                ]
# Find rows which match with closest clinical visits, and take earliest of them
dat_dx2 <- dat_dx[, .SD[min_days==min(min_days)], by=USRDS_ID][,.SD[CLM_FROM == min(CLM_FROM)],by=USRDS_ID]
dat_dx2 <- dat_dx2[min_days < 181, c('USRDS_ID','CLM_FROM','min_days')] # keep those in 180 day window
dat_dx2[, CLM_FROM := as.character(CLM_FROM)]

cols1 <- c('USRDS_ID','CLM_FROM', names(till2009)[str_starts(names(till2009),'HSDIAG')])
cols2 <- c('USRDS_ID','CLM_FROM', names(from2010)[str_starts(names(from2010), 'HSDIAG')])

# Left joins of the discovered clinical dates with the two clinical databases
tx1 = merge(dat_dx2[,c('USRDS_ID','CLM_FROM'), with=F], 
            till2009[,..cols1], by = c('USRDS_ID','CLM_FROM'), 
            all.x=T)[!is.na(HSDIAG1)]
tx2 = merge(dat_dx2[, c('USRDS_ID','CLM_FROM'), with=F], 
            from2010[,..cols2], by = c('USRDS_ID', 'CLM_FROM'), 
            all.x=T)[!is.na(HSDIAG1)]
tx1 <- tx1[!USRDS_ID %in% intersect(tx1$USRDS_ID, tx2$USRDS_ID)] # Common id and date pull from tx2

tx1 <- unique(tx1) # Make sure duplicates are removed
tx2 <- unique(tx2)

# Gather the datasets 
tx11 <- melt(tx1, id.vars = c("USRDS_ID", "CLM_FROM"), variable.name = 'diag', value.name='code')
tx21 <- melt(tx2, id.vars = c('USRDS_ID','CLM_FROM'), variable.name = 'diag', value.name = 'code')

merged_codes <- rbind(tx11, tx21)
# If there are common ids, make sure the earliest of them is taken, given that all the 
# visits should be within 180 days of FIRST_SE
merged_codes <- merged_codes[, .SD[CLM_FROM == min(CLM_FROM)], by=USRDS_ID] 

## Now match to see if the GI.bleed and liver codes reside here. 

merged_codes[, ':=' (GI_present = (code %in% comorb_codes$GI.Bleeding),
                     Liver_present= (code %in% comorb_codes$Liver))]
present_codes <- merged_codes[, list(GI = ifelse(any(GI_present),'Y','N'),
                                     Liver = ifelse(any(Liver_present), 'Y','N')),
                              by=USRDS_ID]

analytic_dt <- merge(analytic_dt, present_codes, by = 'USRDS_ID', all.x = TRUE)

# TODO: Fix this code so that all the comorbidities don't devolve to 0
cmbs <- c('Ihd','Cardia','Cva','Pvasc','COMO_OTHCARD','Pulmon','Cancer','DYSRHYT','GI','Liver', 'DIABETES')
analytic_dt[, (cmbs) := lapply(.SD, function(x) ifelse(is.na(x), 'N', x)), .SDcols = cmbs]
analytic_dt[, (cmbs) := lapply(.SD, function(x) ifelse(x=='Y', 1, 0)), .SDcols = cmbs]

# Compute the comorb_indx
# comorb_indx = ASHD + 3*CHF +
#   2 * (CVATIA + PVD + Other.cardiac + COPD +
#          GI.Bleeding + Liver + Dysrhhythmia + Cancer) +
#   Diabetes

analytic_dt[, comorb_indx := Ihd + 3 * Cardia + 
              2 * (Cva + Pvasc + COMO_OTHCARD + Pulmon + GI + Liver + DYSRHYT + Cancer) + 
              DIABETES]
write_fst(analytic_dt, path(dropdir,'Revision','AnalyticUpdated.fst'))

# Create white subset -----------------------------------------------------

analytic_whites <- analytic_dt[RACE2=='White']
assertthat::are_equal(sum(analytic_dt$RACE2=='White', na.rm=T), nrow(analytic_whites))

write_fst(analytic_whites, path(dropdir, 'Analytic_Whites.fst'))


# Create subset for rest of racial groups for simulation ------------------

analytic_rest <- analytic_dt[RACE2 != 'White']
assertthat::are_equal(sum(analytic_dt$RACE2!='White', na.rm=T), nrow(analytic_rest))
write_fst(analytic_rest, path(dropdir, 'Analytic_Rest.fst'))

