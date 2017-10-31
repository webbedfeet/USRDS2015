#' ---
#' title: Updating survival times to 2015
#' date: `r Sys.Date()`
#' ---

source('lib/reload.R'); reload()
dbdir <- verifyPaths()
dbdir14 <- verifyPaths(2014)
con <- dbConnect(SQLite(), file.path(dbdir, 'USRDS.sqlite3'))

P <- tbl(con, 'StudyIDs') %>% left_join(tbl(con, 'patients'))
M <- tbl(con, 'StudyIDs') %>% left_join(tbl(con, 'medevid'))

P %>% count() # 1131409
M %>% count() # 1159490

P %>% select(USRDS_ID, DIED, FIRST_SE, TX1DATE) %>% 
  mutate(tod = (DIED - FIRST_SE)/365.25,
         tot = (TX1DATE - FIRST_SE)/365.25) %>% 
  collect()  -> dat_p

con14 <- dbConnect(SQLite(), file.path(dbdir14, 'PR_db'))
analytic_data <- tbl(con14,'AnalyticData') %>% collect()
dbDisconnect(con14)

bl <- analytic_data %>% select(USRDS_ID, DIED, FIRST_SE, TX1DATE, tod, tot) %>% left_join(dat_p, by='USRDS_ID')

dbDisconnect(con)