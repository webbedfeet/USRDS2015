# Developing parametric survival models for discontinuation and 
# transplant outcomes for the white subgroup

# Setup -------------------------------------------------------------------

abhiR::reload()

dropdir <- path(find_dropbox(), 'NIAMS','Ward','USRDS2015','data')
# analytic_data <- read_fst(path(dropdir,'Analytic.fst'))

dbdir <- verifyPaths()
sql_conn <- dbConnect(SQLite(), file.path(dbdir,'USRDS.sqlite3'))

till2009 <- tbl(sql_conn, 'till2009')
from2010 <- tbl(sql_conn,'from2010')
studyids <- tbl(sql_conn, 'StudyIDs')
analytic <- tbl(sql_conn, 'AnalyticData')

# "create table hosp1 as select * from till2009 where USRDS_ID in (select USRDS_ID from Whites)"
# "create table hosp2 as select * from from2010 where USRDS_ID in (select USRDS_ID from Whites)"

hosp1 <- tbl(sql_conn, 'hosp1')
hosp2 <- tbl(sql_conn, 'hosp2')
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


# Create white subset -----------------------------------------------------

analytic_whites <- analytic_data %>% filter(RACE2 == 'White')
assertthat::are_equal(sum(analytic_data$RACE2=='White', na.rm=T), nrow(analytic_whites))
analytic_whites <- analytic_whites %>% 
  mutate(cens_type2 = case_when(
    cens_type == 0 ~ "Lost to followup",
    cens_type == 1 ~ "Dead",
    cens_type == 2 ~ "Transplant",
    cens_type == 3 ~ "Discontinued",
    TRUE ~ NA_character_)) # NA of type character

# Adding DISGRPC codes to analytic data --------------------------------------------

disgrpc_code <- readRDS(path(dropdir, 'disgrpc_code.rds')) # See cause_esrd.R
disgrpc <- fst(path(dropdir, 'raw_data.fst'))[,c('USRDS_ID','DISGRPC')] %>%
  distinct() %>%
  left_join(disgrpc_code, by=c('DISGRPC'='Format')) %>%
  mutate(Description = ifelse(Description %in% c('8','**OTHER**'), NA, Description)) %>%
  mutate(Description = fct_other(Description,keep=c('Diabetes','Hypertension','Glomeruloneph.'))) %>%
  rename(ESRD_Cause = Description)
analytic_whites <- analytic_whites %>% left_join(disgrpc) # Adding reason for dialysis


# Adding comorbidity index ------------------------------------------------
comorb_codes <- list(
  'ASHD' = '410-414, V4581, V4582',
  'CHF' = '39891, 422, 425, 428, 402X1,404X1, 404X3, V421',
  'CVATIA' = '430-438',
  'PVD' = '440-444, 447, 451-453, 557',
  'Other.cardiac' = '420-421, 423-424, 429, 7850-7853,V422,V433',
  'COPD' = '491-494, 496, 510',
  'GI.Bleeding' = '4560-4562, 5307, 531-534, 56984, 56985,578',
  'Liver' = '570-571,5721, 5724,5731-5733,V427',
  'Dysrhhythmia' = '426-427,V450, V533',
  'Cancer' = '140-172, 174-208, 230-231, 233-234',
  'Diabetes' = '250, 3572, 3620X, 36641'
) %>% map(icd9_codes)

determine_comorbs <- function(d){
  d %>% select(USRDS_ID,starts_with("CLM"), starts_with("HSDIAG")) %>% 
    gather(DIAG, codes, starts_with("HSDIAG")) %>% 
    bind_cols(as.data.frame(lapply(comorb_codes, function(x) .$codes %in% x))) %>% 
    select(-DIAG, -codes) %>% 
    group_by(USRDS_ID, CLM_FROM,CLM_THRU) %>% 
    summarise_all(any) %>% 
    ungroup()
}

white <- analytic %>% filter(RACE2=='White') %>% select(USRDS_ID)
d1 <- till2009 %>% select(USRDS_ID, starts_with("CLM"), starts_with("HSDIAG")) %>% 
  inner_join(white) %>% collect(n = Inf)
d2 <- from2010 %>% select(USRDS_ID, starts_with("CLM"), starts_with("HSDIAG")) %>% 
  inner_join(white) %>% collect(n=Inf)
library(data.table)
d <- as.data.table(bind_rows(d1,d2)); rm(d1, d2); gc()

impute_ones <- function(x){
  if(any(x)){
    n <- length(x)
    i <- min(which(x)) # Assumes T/F variable
    x[i:n] <- TRUE
    return(x)
  } else{
    return(x)
  }
}
u <- d[USRDS_ID %in% c(3892, 27132, 50154)]



ids <- unique(d$USRDS_ID)
grps <- sample(1:5, length(ids), replace=T)

d2 <- d[USRDS_ID %in% ids[grps==1]]


f2 <- function(d2){
  d21 <- melt(d2, c('USRDS_ID','CLM_FROM','CLM_THRU'), 
              variable.name = 'DIAG', value.name = 'codes')
  bl <- d21[,lapply(comorb_codes, f, codes)]
  d21 <- cbind(d21[,c('USRDS_ID','CLM_FROM','CLM_THRU')], bl)
  d31 <- d21[, lapply(.SD, any, na.rm=T), by=.(USRDS_ID, CLM_FROM, CLM_THRU)]
  d31 <- d31[, lapply(.SD, impute_ones), by=.(USRDS_ID), .SDcols = names(comorb_codes)]
  d31[,comorb_indx := ASHD + 3*CHF + 
        2 * (CVATIA + PVD + Other.cardiac + COPD + 
               GI.Bleeding + Liver + Dysrhhythmia + Cancer) +
        Diabetes]
  d41 <- d31[,.(comorb_indx=max(comorb_indx)), by=.(USRDS_ID)]
  return(d41)
}


f1 <- function(u){
  u1 <- melt(u, c('CLM_FROM','CLM_THRU'),
             variable.name = 'DIAG', value.name = 'codes')
  bl <- u1[,lapply(comorb_codes, f, codes)]
  u1 <- cbind(u1[,c('CLM_FROM','CLM_THRU')], bl)
  u2 <- u1[,lapply(.SD, any, na.rm=T), by=c('CLM_FROM','CLM_THRU')]
  u3 <- u2[,lapply(.SD, impute_ones), .SDcols = names(comorb_codes)]
  u3[,comorb_index := ASHD + 3*CHF + 
       2 * (CVATIA + PVD + Other.cardiac + COPD + 
              GI.Bleeding + Liver + Dysrhhythmia + Cancer) +
       Diabetes]
  out <- u3[, .(comorb_index = max(comorb_index))]
  return(out)
}

D <- d[,f1(.SD), by=.(USRDS_ID)]
h()
f <- function(x,y) y %in% x
f1 <- function(d){
  d1 <- d %>% gather(DIAG, codes, starts_with("HSDIAG")) 
  bl <- map_dfc(comorb_codes, ~D$codes %in% .)
  d1 <- cbind(select(d1, USRDS_ID:CLM_THRU), bl) %>% 
    group_by(vars(USRDS_ID:CLM_THRU)) %>% 
    summarize_all(any)
  return(d1)
}

tst <- d %>% split(., .$USRDS_ID) %>% 
  map_df(f1)
  
  d1 <- melt(d, c('USRDS_ID','CLM_FROM','CLM_THRU'), 
             variable.name='DIAG',value.name='codes')
  bl <- d1[,lapply(comorb_codes, f, codes )]
  d1 <- cbind(d1[,c("USRDS_ID",'CLM_FROM','CLM_THRU')], bl)
  d2 <- d1[, ]
}



D <- melt(d, c('USRDS_ID','CLM_FROM','CLM_THRU'), variable.name = 'DIAG',
          value.name = 'codes')
blah <- D[,lapply(comorb_codes, f, codes)]
D <- cbind(D[,c("USRDS_ID",'CLM_FROM','CLM_THRU')], blah)

D[,"ASHD" := codes %in% comorb_codes[['ASHD']]]
D <- d %>% gather(DIAG, codes, starts_with("HSDIAG"))
blah <- map_dfc(comorb_codes, ~D$codes %in% .x)
D <- bind_cols(D, blah) %>% select(-DIAG, -codes) %>% 
  group_by(vars(USRDS_ID:CLM_THRU)) %>% 
  summarize_all(any) %>% 
  ungroup(); rm(blah); gc()

for(n in names(comorb_codes)){
  D[,n] <- D$codes %in% comorb_codes[[n]]
}

blah <- map_dfc(comorb_codes, ~D$codes %in% .x) %>% bind_cols(D$USRDS_ID)
write_fst(d, 'tmp_hosp.fst')

# length(setdiff(analytic_data$USRDS_ID, d$USRDS_ID))/length(unique(analytic_data$USRDS_ID))

u <- determine_comorbs(d)

write_fst(analytic_whites, path(dropdir, 'Analytic_Whites.fst'))

