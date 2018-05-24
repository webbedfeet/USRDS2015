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


# Extract data for patients with each index condition -----------------------------------------
d <- readRDS('data/rda/tmpComorb.rds')


query_icd9 <- function(...){
  x <- as.character(sys.call())[-1]
  x <- x %>% 
    str_split('-') %>% 
    map(str_trim) %>%
    map(create_seq) %>% 
    map(as.character) %>% 
    unlist() %>% str_c('^',.)
    
  return(x)
}
create_seq <- function(x){
  if(any(is.na(as.numeric(x)))) return(x)
  seq(x[1], x[2])
}
tst = '141-144'
