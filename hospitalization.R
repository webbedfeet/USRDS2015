# Script to interrogate the hospital databases to extract IDs
# of individuals who had prevalent comorbidities. This will be matched against
# IDs of people discontinuing dialysis to see how the race pattern appears.

ProjTemplate::reload()
dbdir = verifyPaths()
sql_conn = dbConnect(SQLite(), file.path(dbdir,'USRDS.sqlite3'))

till2009 <- tbl(sql_conn, 'till2009')
from2010 <- tbl(sql_conn, 'from2010')
dbs <- list(till2009, from2010)

# Stroke ------------------------------------------------------------------

stroke <- dbs %>%
  lapply(., function(db){
    db %>%
      select(USRDS_ID, starts_with('HSDIAG'))%>%
      mutate(PRIM = substr(HSDIAG1,1,3)) %>%
      filter(PRIM == '430' | PRIM=='431' | PRIM == '432' | PRIM == '433' | PRIM=='434') %>%
      collect()
  })

stroke_primary <- stroke %>%
  lapply(., function(db) db %>%  select(USRDS_ID)) %>% bind_rows() %>% distinct()
head(stroke_primary)
# Stroke with complications -----------------------------------------------

stroke_compl <- stroke %>%
  lapply(., function(db){
    db %>% gather(diag, code, -USRDS_ID, -HSDIAG1) %>%
      mutate(code1 = substr(code,1,3)) %>%
      filter(code1 %in% c('438','342','344')) %>%
      select(USRDS_ID) %>%
      distinct()
  }) %>%
  bind_rows() %>%
  distinct()

head(stroke_compl)

# Lung cancer -------------------------------------------------------------

LuCa <- dbs %>%
  lapply(., function(db){
    db %>% mutate(PRIM = substr(HSDIAG1,1,3)) %>%
      filter(PRIM=='162') %>% select(USRDS_ID) %>% collect()
  }) %>%
  bind_rows() %>% distinct()
head(LuCa)
# Metastatic cancer -------------------------------------------------------

MetsCa <- dbs %>%
  lapply(., function(db){
  db %>% mutate(PRIM=substr(HSDIAG1,1,3)) %>%
    filter(PRIM== '196' | PRIM == '197' | PRIM == '198' | PRIM == '199') %>%
    select(USRDS_ID) %>% collect()}) %>%
  bind_rows() %>%
  distinct()
head(MetsCa)
# Dementia ----------------------------------------------------------------

## I'm moving this to Python since it's much faster at processing the database
## row-wise. However, I need to get some SQL calls generated here.

# till2009 %>% select(USRDS_ID, starts_with('HSDIAG')) %>% show_query()
# from2010 %>% select(USRDS_ID, starts_with("HSDIAG")) %>% show_query()

# dementia <- read_csv('data/Dementia.csv')
# names(dementia) <- 'USRDS_ID'
# head(dementia)

library(DBI)
sql1 <- paste(capture.output(till2009 %>% 
                               select(USRDS_ID, starts_with('HSDIAG')) %>% 
                               show_query(), type='message')[-1], collapse=' ')
sql2 <- paste(capture.output(from2010 %>% 
                               select(USRDS_ID, starts_with('HSDIAG')) %>% 
                               show_query(), type='message')[-1], collapse=' ')
sqlist <- list(sql1,sql2)


dement <- list()
for (sql in sqlist){
  rs <- dbSendQuery(sql_conn, sql)
  while(!dbHasCompleted(rs)){
    d <-  dbFetch(rs, n = 10000)
    dement <-  c(dement, d %>% gather(hsdiag, code, -USRDS_ID) %>% 
                   filter(str_detect(code, '^290|^2941|^331[012]')) %>% 
                   select(USRDS_ID) %>% 
                   distinct())
  }
  dbClearResult(rs)
}

dement <- data.frame(USRDS_ID = sort(unique(unlist(dement))))

# Failure to thrive -------------------
## This can appear in any of the diagnoses

sql1 <- paste(capture.output(till2009 %>% 
                               select(USRDS_ID, starts_with('HSDIAG')) %>% 
                         show_query(), type='message')[-1], collapse=' ')
sql2 <- paste(capture.output(from2010 %>% 
                               select(USRDS_ID, starts_with('HSDIAG')) %>% 
                               show_query(), type='message')[-1], collapse=' ')
sqlist <- list(sql1,sql2)

thrive = list()
for (sql in sqlist) {
  rs <- dbSendQuery(sql_conn, sql)
  while (!dbHasCompleted(rs)) {
    d <- dbFetch(rs, n = 10000)
    thrive <- c(thrive,
                d %>% gather(hsdiag, code, -USRDS_ID) %>% 
                  filter(str_detect(code, '783[237]')) %>% 
                  select(USRDS_ID) %>% 
                  distinct())
  }
  dbClearResult(rs)
}
thrive <- data.frame(USRDS_ID = sort(unique(unlist(thrive))))

dbDisconnect(sql_conn); gc()

hospitalization <- list('stroke_primary' = stroke_primary, 
                        'stroke_compl' = stroke_compl,
                        'LuCa' = LuCa,
                        'MetsCa' = MetsCa,
                        'dement' = dement,
                        'thrive' = thrive)
saveRDS(hospitalization, file = 'data/hospitalization_ids.rds')


# Verifying incident cases from medevid ------------------------------------

sql_conn <- dbConnect(SQLite(), file.path(dbdir, 'USRDS.sqlite3'))
medevid <- tbl(sql_conn, 'medevid')
studyids <- tbl(sql_conn, 'StudyIDs')
medevid %>% inner_join(studyids) %>% count()
joined_tbl <- medevid %>% inner_join(studyids)

bl <- medevid %>% inner_join(studyids) %>% collect(n=5)
bl2 <- joined_tbl %>% select(USRDS_ID,COMO_CVATIA, CANCER, COMO_CANC, CTDATE, DIALDAT, DIALEDT, DIED) %>% 
  collect(n=Inf) %>% 
  filter(COMO_CVATIA !='') 
bl2 <- bl2 %>% 
  group_by(USRDS_ID) %>% 
  top_n(-1, DIALDAT) %>% 
  ungroup()
