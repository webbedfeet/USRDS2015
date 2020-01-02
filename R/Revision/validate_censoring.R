## Exploration of censoring times for USRDS data
##
## The current method censors at the first time the RXHIST60 file shows a
## X or Z in the RXGROUP variable (type of treatment modality), or the end of the
## study (2015-01-01). We may want to actually censor at the time of last known
## treatment

abhiR::reload()
dropdir <- here('data')
analytic <- read_fst(path(dropdir, 'Revision',"AnalyticUpdated.fst"))

analytic <- analytic %>%
  mutate(surv_date = FIRST_SE + (365.25 * surv_time))

sql_conn <- dbConnect(SQLite(), path(dropdir,'raw','USRDS.sqlite3'))
rxhist <- tbl(sql_conn, 'StudyIDs') %>% left_join(tbl(sql_conn, 'rxhist60')) %>%
  collect(n = Inf)

last_tx <- rxhist %>%
  # mutate_at(vars(BEGDATE, ENDDATE), as.Date) %>%
  group_by(USRDS_ID) %>%
  summarize(tx_start = min(BEGDATE, na.rm=T),
            tx_date = max(ENDDATE, na.rm=T)) %>%
  ungroup() %>%
  mutate(tx_date = as.Date(tx_date),
         tx_start = as.Date(tx_start))
head(last_tx)

test = analytic %>% select(USRDS_ID, surv_date, cens_type2) %>%
  left_join(last_tx)

test %>% filter(cens_type2=='Lost to followup') %>%
  mutate(flag = surv_date > tx_date) %>%
  summarize(mean(flag, na.rm=T))
test %>% filter(cens_type2 == 'Lost to followup') %>%
  mutate(flag = surv_date < '2015-01-01') %>%
  summarize(mean(flag, na.rm=T))


# Checking transplant models ----------------------------------------------

analytic_filt <- analytic %>%
  filter(RACE2 %in% c('White','Black','Hispanic','Asian','Native American'))
