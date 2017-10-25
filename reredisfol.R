# Looking at reasons for discontinuation
datadir2014 <- "P:/Mohammed"
source('lib/reload.R'); reload()
dbdir <- verifyPaths() # is the database directories available
if (is.na(dbdir)) stop('Databases not accessible')
sql_conn <- src_sqlite(file.path(dbdir,'USRDS.sqlite3'))

# con1 = dbConnect(MonetDBLite(), dbdir)
# mdb_conn <- src_monetdblite(dbdir)
# # conn1 <- dbConnect(MonetDBLite(), dbdir)

sql_2014 <- src_sqlite(file.path(datadir2014,'ALL Files', 'PR_db'))
study_tbl <- tbl(sql_2014, 'AnalyticData')
withdrew <- study_tbl %>% select(USRDS_ID, cens_type) %>% filter(cens_type==3) %>% collect()

deaths_db = tbl(sql_conn, 'death')
deaths_db %>% count(REREDISFOL) -> discontinue_summary

bl <- deaths_db %>% select(USRDS_ID, REREDISFOL) %>% collect()
withdrew <- withdrew %>% left_join(bl)

withdrew %>% count(REREDISFOL) %>% 
  mutate(perc = 100*(n/sum(n)))  %>% 
  mutate(REREDISFOL = c('','HD/PD access failure','Failure to thrive',
                        'Acute medical complication','Other','Unknown', 'Missing'))
  


