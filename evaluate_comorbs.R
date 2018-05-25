##%######################################################%##
#                                                          #
#### Extracting comorbidities from hospitalization data ####
#                                                          #
##%######################################################%##

ProjTemplate::reload()
dbdir <- verifyPaths()
dropdir <- file.path(find_dropbox(),'NIAMS','Ward','USRDS2015','data')
sql_conn <- dbConnect(SQLite(), file.path(dbdir,'USRDS.sqlite3'))

till2009 <- tbl(sql_conn, 'till2009')
from2010 <- tbl(sql_conn,'from2010')
studyids <- tbl(sql_conn, 'StudyIDs')
hospitalization <- readRDS('data/rda/final_hosp_data.rds')


# Add index condition patient ids in DB -------------------------------------------------------

for(n in names(hospitalization)){
  dbWriteTable(sql_conn, n, select(hospitalization[[n]], USRDS_ID), overwrite = TRUE)
}

# Extract relevant records from hospitalization database --------------------------------------

for(n in names(hospitalization)){
  blah = vector('list', 2)
  blah[[1]] <- till2009 %>% select(USRDS_ID, starts_with("CLM"), contains("DIAG")) %>% 
    inner_join(tbl(sql_conn, n) %>% select(USRDS_ID)) %>% collect(n=Inf)
  blah[[2]] <- from2010 %>% select(USRDS_ID, starts_with("CLM"), contains("DIAG")) %>% 
    inner_join(tbl(sql_conn, n) %>% select(USRDS_ID)) %>% collect(n = Inf)
  saveRDS(blah, file = paste0('data/rda/',n,'.rds'))
  rm(blah)
  gc()
}


# Exemplar data that will be extended for full query


d <- till2009 %>% collect(n=10000)
saveRDS(d, 'data/rda/tmpComorb.rds', compress = T)
dbDisconnect(sql_conn)


# ICD-9 codes for each comorbidity ------------------------------------------------------------

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


# Extract data for patients with each index condition -----------------------------------------
# d <- readRDS('data/rda/tmpComorb.rds')

determine_comorbs <- function(d){
  d %>% select(USRDS_ID,starts_with("CLM"), starts_with("HSDIAG")) %>% 
    gather(DIAG, codes, starts_with("HSDIAG")) %>% 
    bind_cols(as.data.frame(lapply(comorb_codes, function(x) .$codes %in% x))) %>% 
    select(-DIAG, -codes) %>% 
    group_by(USRDS_ID, CLM_FROM,CLM_THRU) %>% 
    summarise_all(any) %>% 
    ungroup()
}

datafiles <- paste0('data/rda/',names(hospitalization),'.rds')
index_condn_comorbs <- list()
for(f in datafiles){
  d <- readRDS(f)
  print(paste('Reading', basename(f),'...'))
  index_condn_comorbs[[str_remove(basename(f), '.rds')]] <- 
    bind_rows(map(d, determine_comorbs))
}

saveRDS(index_condn_comorbs, file.path(dropdir, 'index_condn_comorbs.rds'), compress=T)

# Mutate comorbs so that once you get disease, you stay a 1 from then on ----

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

for(i in 1:length(index_condn_comorbs)){
  print(paste0('Working on ', names(index_condn_comorbs)[i]))
  d <- index_condn_comorbs[[i]]
  index_condn_comorbs[[i]] <- d %>% 
    arrange(USRDS_ID, CLM_FROM) %>% 
    group_by(USRDS_ID) %>% 
    mutate_at(vars(ASHD:Diabetes), impute_ones) %>% 
    ungroup()
}



# Compute USRDS comorb score at each hospitalization ----
# Based on Table 2 of Liu, et al, 
# Kidney International (2010) 77, 141–151; doi:10.1038/ki.2009.413

d <- d %>% 
  mutate(comorb_indx = ASHD + 3*CHF + 
           2 * (CVATIA + PVD + Other.cardiac + COPD + 
                  GI.Bleeding + Liver + Dysrhhythmia + Cancer) +
           Diabetes)

# TODO: Determine baseline status for GI and Liver
# TODO: Determine status at time of index condition
