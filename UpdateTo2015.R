#' ---
#' title: Updating survival times to 2015
#' date: `r Sys.Date()`
#' ---

ProjTemplate::reload()
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
saveRDS(analytic_data, 'data/rda/analytic14.rds')

bl <- analytic_data %>% select(USRDS_ID, DIED, FIRST_SE, TX1DATE, tod, tot) %>% left_join(dat_p, by='USRDS_ID')



# Grab discontinuation times ----------------------------------------------

#' The `rxhist` table gives the treatments received by patients at different times during their time in the study
#' What we do here is look at the sequential treatment codes, and find situations where there is discontinuation (`rxgroup=="B"`)
#' which is not followed by any dialysis, which means that is the final discontinuation.
tbl(con, 'StudyIDs') %>% left_join(tbl(con,'rxhist'), by = c("USRDS_ID" = "usrds_id")) %>% collect() -> rxhist # Restricted to earlier study
txpattern <- rxhist %>% 
  arrange(USRDS_ID, BEGDATE) %>% 
  group_by(USRDS_ID) %>% 
  summarise(tx = paste(rxgroup, collapse = '')) # Grabs pattern of treatment
txpattern <- txpattern %>% 
  filter(str_detect(tx, 'B')) %>% # Had a discontinuation
  filter(!str_detect(str_match(tx, '.+[B](.*)')[,2], '[0-9]')) # but no dialysis after the last one
rxhist %>% filter(USRDS_ID %in% txpattern$USRDS_ID) %>% 
  filter(rxgroup == 'B') %>% 
  group_by(USRDS_ID) %>% 
  filter(BEGDATE == max(BEGDATE)) %>% 
  ungroup() -> blah # Grab the last discontinuation record for each person

#' There may be an issue with the origin date of the dates here, so we'll test it out to see which origin matches our
#' earlier analysis

origin <-  '1970-01-01'
blah <- blah %>% 
  mutate(begin_withdrawal = as.Date(BEGDATE, origin = origin)) %>% 
  select(USRDS_ID, begin_withdrawal)
d_withdraw <- dat_p %>% left_join(blah) %>% 
  mutate(DIED = as.Date(DIED, origin = origin),
         FIRST_SE = as.Date(FIRST_SE, origin = origin),
         TX1DATE = as.Date(TX1DATE, origin = origin))
P %>% 
  mutate(withdraw = if(RXSTOP=='A' | RXSTOP=='C' | RXSTOP == 'D' | RXSTOP=='E') 1 else 0) %>% 
  select(USRDS_ID, withdraw) %>% collect() -> withdraw_indic


dbDisconnect(con)