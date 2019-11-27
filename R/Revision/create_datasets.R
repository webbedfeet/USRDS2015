# Code for getting white and rest analytic datasets for simulation study

# Setup -------------------------------------------------------------------

abhiR::reload()

# dropdir <- path(find_dropbox(), 'NIAMS','Ward','USRDS2015','data')
dropdir <- "P:/Ward/USRDS2015/data"
dir_exists(dropdir)
analytic_data <- read_fst(path(dropdir,'Analytic.fst'))

# dbdir <- verifyPaths()
## Following paths are for MBP
sql_conn <- dbConnect(SQLite(), file.path('data','raw','USRDS.sqlite3'))

till2009_db <- tbl(sql_conn, 'till2009')
from2010_db <- tbl(sql_conn,'from2010')
studyids_db <- tbl(sql_conn, 'StudyIDs')
analytic_db <- tbl(sql_conn, 'AnalyticData')

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
  )) %>%
  mutate(REGION = as.factor(REGION))


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
sql_conn <- dbConnect(SQLite(), 'data/raw/USRDS.sqlite3')
medevid <- studyids_db %>% left_join(tbl(sql_conn, 'medevid')) %>%
  select(USRDS_ID, ALBUM, ALBUMDT, ALBUMLM,ALCOH, DRUG) %>%
  collect(n = Inf) %>%
  mutate_at(vars(ALCOH, DRUG), ~ifelse(. == '', NA, .))
medevid1 <- medevid %>% left_join(analytic_data %>% select(USRDS_ID, FIRST_SE)) %>%
  filter_at(vars(ALCOH:DRUG), ~!is.na(.)) %>% # There are no missings
  mutate(ALBUMDT = as.Date(ALBUMDT),
         time_from_se = abs(FIRST_SE - ALBUMDT))
bl <- filter(medevid1, !is.na(ALBUM))

## Deal with the situations where an individual has more than one row of data
bl %>% count(USRDS_ID) %>% filter(n > 1) %>% left_join(bl) -> bl2
bl2 %>%
  mutate_at(vars(ALCOH, DRUG), ~ifelse(.=='Y', 1, 0)) %>%
  group_by(USRDS_ID) %>%
  filter(time_from_se == min(time_from_se, na.rm=T)) %>% # Grab first dialysis time
  mutate(ALBUM = mean(ALBUM, na.rm=T)) %>%
  mutate_at(vars(ALCOH, DRUG), ~max(., na.rm=T)) %>%  # Ever ALCOH or DRUG
  ungroup() %>%
  select(-ALBUMLM, -ALBUMDT) %>%
  distinct() -> bl3

## Now merge back together with singletons
medevid1 <- medevid1 %>%
  count(USRDS_ID) %>%
  filter(n==1) %>%
  left_join(medevid1) %>%
  select(-ALBUMDT, -ALBUMLM) %>%
  mutate_at(vars(ALCOH, DRUG), ~ifelse(.=='Y',1,0)) %>%
  bind_rows(bl3) %>%  # <- merge back the flattened duplicates
  select(-n, -time_from_se)

## Missing value evaluation

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

# abhiR::reload()
# dropdir <- path(find_dropbox(), 'NIAMS','Ward','USRDS2015','data')


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
bl1 <- new_comorbs[,.N,by=USRDS_ID][N==1][new_comorbs, on="USRDS_ID", nomatch=0] # Keep only singletons
bl1[,N := NULL] # Remove N
bl1[bl1==''] <- NA
bl2 <- new_comorbs[, .N, by=USRDS_ID][N>1][new_comorbs, on="USRDS_ID", nomatch =0] # Work on multiples
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

save.image(file='tmp.rda', compress=T)
### I'm reading, then left-joining with study ids, then removing rows with missing values
### TODO


sql_conn <- dbConnect(SQLite(), 'data/raw/USRDS.sqlite3')
studyids_db <- tbl(sql_conn, 'StudyIDs')
till2009_db <- tbl(sql_conn, 'till2009')
from2010_db <- tbl(sql_conn, 'from2010')

bl1 <- studyids_db %>% left_join(till2009_db) %>% filter(!is.na(CLM_FROM)) %>%
  select(USRDS_ID, CLM_FROM, CLM_THRU) # Can't put HSDIAG here since there are
# different numbers of variables in till2009 and from 2010

bl2 <- studyids_db %>% left_join(from2010_db) %>% filter(!is.na(CLM_FROM)) %>%
  select(USRDS_ID, CLM_FROM, CLM_THRU)
bl <- union(bl1, bl2) %>% collect(n=Inf)
dbDisconnect(sql_conn)
bl <- bl %>% left_join(analytic_dt %>%
                         mutate(FIRST_SE = as.character(FIRST_SE)) %>%
                         select(USRDS_ID, FIRST_SE))
bl <- mutate_if(bl, is.character, as.Date)
bl <- setDT(bl)
setkey(bl, 'USRDS_ID','CLM_FROM')
bl[,':='(tt_from=abs(FIRST_SE-CLM_FROM),
         tt_thru = abs(FIRST_SE-CLM_THRU))][
           ,min_days := as.numeric(pmin(tt_from, tt_thru))
         ]
dat_dx <- bl
# This gives the dataset with all clinical visits along with the FIRST_SE

# Find rows which match with closest clinical visits, and take earliest of them
dat_dx2 <- dat_dx[, .SD[min_days==min(min_days)], by=USRDS_ID][,.SD[CLM_FROM == min(CLM_FROM)],by=USRDS_ID]
dat_dx2 <- dat_dx2[min_days < 181, c('USRDS_ID','CLM_FROM','min_days')] # keep those in 180 day window
dat_dx2[, CLM_FROM := as.character(CLM_FROM)] # Transform CLM_FROM to character for merges

## Now add the diagnoses codes. We need to do this separately for till2009
## and from2010

sql_conn <- dbConnect(SQLite(), 'data/raw/USRDS.sqlite3')
till2009_db <- tbl(sql_conn,'till2009')
from2010_db <- tbl(sql_conn, 'from2010')
sid <- tbl(sql_conn, 'StudyIDs')

till2009 <- sid %>% left_join(till2009_db) %>%
  select(USRDS_ID, CLM_FROM, CLM_THRU, starts_with("HSDIAG")) %>%
  collect(n=Inf)
from2010 <- sid %>% left_join(from2010_db) %>%
  select(USRDS_ID, CLM_FROM, CLM_THRU, starts_with("HSDIAG")) %>%
  collect(n=Inf)
dbDisconnect(sql_conn)

till2009 <- setDT(till2009)
from2010 <- setDT(from2010)
setkey(till2009, 'USRDS_ID','CLM_FROM')
setkey(from2010, 'USRDS_ID','CLM_FROM')
setkey(dat_dx2, 'USRDS_ID','CLM_FROM')

## NOTE: The hospital data (till2009 and from2010) only goes till 2013-12-31,
## while the FIRST_SE data goes till 2014-12-31. This has been verified from the
## original sas7bdat files.
##
save(from2010, till2009, analytic_dt, dat_dx2, file='tmp2.rda', compress=T)
# Left joins of the discovered clinical dates with the two clinical databases
# There is an intersection of 1404 IDs between the two merged datasets below.
# I have confirmed that all the intersection records are duplications in tx2, and
# not disparate records. So I'll just keep the data from tx1 and filter the
# common IDs from tx2
tx1 = merge(dat_dx2,
            till2009, by = c('USRDS_ID','CLM_FROM'),
            all.x=T)[!is.na(HSDIAG1)]
tx2 = merge(dat_dx2,
            from2010, by = c('USRDS_ID', 'CLM_FROM'),
            all.x=T)[!is.na(HSDIAG1)][!(USRDS_ID %in% tx1$USRDS_ID)]

assertthat::are_equal(length(intersect(tx1$USRDS_ID, tx2$USRDS_ID)),
                      0)
tx1 <- unique(tx1); tx2 <- unique(tx2)

## NOTE: There are some IDs with multiple rows in each data set. I have verified
## that all the multiple rows arise from the same CLM_FROM date and have
## different CLM_THRU dates. So we're basically looking at the same clinical
## visit, but with different durations recorded for different diagnoses.
## For our purposes this won't matter since we will look at the sum total of
## diagnosis codes for the visit, defined by CLM_FROM
##

# Gather the datasets
tx11 <- melt(tx1, id.vars = c("USRDS_ID", "CLM_FROM"), variable.name = 'diag', value.name='code')
tx21 <- melt(tx2, id.vars = c('USRDS_ID','CLM_FROM'), variable.name = 'diag', value.name = 'code')

merged_codes <- rbind(tx11, tx21)

## Now match to see if the GI.bleed and liver codes reside here.

merged_codes[, ':=' (GI_present = (code %in% comorb_codes$`GI Bleeding`),
                     Liver_present= (code %in% comorb_codes$Liver))]
present_codes <- merged_codes[, list(GI = ifelse(any(GI_present),'Y','N'),
                                     Liver = ifelse(any(Liver_present), 'Y','N')),
                              by=USRDS_ID]

analytic_dt <- merge(analytic_dt, present_codes, by = 'USRDS_ID', all.x = TRUE)
saveRDS(analytic_dt, 'data/Revision/analytic_dt.rds', compress=T)
# TODO: Fix this code so that all the comorbidities don't devolve to 0
cmbs <- c('Ihd','Cardia','Cva','Pvasc','COMO_OTHCARD','Pulmon','Cancer','DYSRHYT','GI','Liver', 'DIABETES')
transform_fn <- function(x){
  if(is.factor(x)) x <- as.character(x) # This is the crucial step
  x <- ifelse(is.na(x), 'N', x)
  x <- ifelse(x == 'Y', 1, 0)
}
analytic_dt[, (cmbs) := lapply(.SD, transform_fn), .SDcols = cmbs]

# Compute the comorb_indx
# comorb_indx = ASHD + 3*CHF +
#   2 * (CVATIA + PVD + Other.cardiac + COPD +
#          GI.Bleeding + Liver + Dysrhhythmia + Cancer) +
#   Diabetes

analytic_dt[, comorb_indx := Ihd + 3 * Cardia +
              2 * (Cva + Pvasc + COMO_OTHCARD + Pulmon + GI + Liver + DYSRHYT + Cancer) +
              DIABETES]
write_fst(analytic_dt, 'data/Revision/AnalyticUpdated.fst')
# write_fst(analytic_dt, path(dropdir,'Revision','AnalyticUpdated.fst'))

# Create white subset -----------------------------------------------------

analytic_whites <- analytic_dt[RACE2=='White']
assertthat::are_equal(sum(analytic_dt$RACE2=='White', na.rm=T), nrow(analytic_whites))

write_fst(analytic_whites, 'data/Revision/Analytic_Whites.fst')
# write_fst(analytic_whites, path(dropdir, 'Revision','Analytic_Whites.fst'))


# Create subset for rest of racial groups for simulation ------------------

analytic_rest <- analytic_dt[RACE2 != 'White']
assertthat::are_equal(sum(analytic_dt$RACE2!='White', na.rm=T), nrow(analytic_rest))
write_fst(analytic_rest, path(dropdir, 'Analytic_Rest.fst'))

