#' ---
#' title: "Inconsistencies between 2014 and 2015 data"
#' author: "Abhijit Dasgupta"
#' output: html_document
#' ---
#'
#' # Background
#' 
#' We want to update 2014 data on which we generated competing risk models for death while
#' on dialysis with 2015 data that should update the timelines of existing individuals 
#' in our study. However, there appear to be several discrepant observations between the 
#' 2014 and 2015 datasets. 
#' 
#+ preamble, echo=FALSE, cache=FALSE, message=FALSE
ProjTemplate::reload()
dbdir <- verifyPaths()
dbdir14 <- verifyPaths(2014)
con <- dbConnect(SQLite(), file.path(dbdir, 'USRDS.sqlite3'))
con2 <- dbConnect(SQLite(), file.path(dbdir14, 'USRDS14.sqlite3'))
con3 <- dbConnect(SQLite(), file.path(dbdir14, 'PR_db'))
knitr::opts_chunk$set(echo=FALSE)

## Add study ids to the 2014 DB
studyids = dbGetQuery(con, 'select * from StudyIDs')
dbWriteTable(con2, 'StudyIDs', studyids, overwrite=TRUE)

#+ setup, echo = FALSE, cache=TRUE, message=FALSE
analytic_data <- readRDS('data/rda/analytic14.rds')

P1 <- tbl(con, 'StudyIDs') %>% left_join(tbl(con, 'patients')) %>% 
  select(USRDS_ID, FIRST_SE, TX1DATE, DIED) %>% collect(n = Inf)
analytic_data <- analytic_data %>% 
  mutate_at(vars(FIRST_SE, TX1DATE, DIED), as.Date, origin = '1960-01-01') %>% 
  mutate_at(vars(FIRST_SE, TX1DATE, DIED), as.character)
dbDisconnect(con)

d <- analytic_data %>% select(USRDS_ID, FIRST_SE, TX1DATE, DIED) %>% 
  rename(FIRST_SE14 = FIRST_SE, TX1DATE14 = TX1DATE, DIED14 = DIED) %>% 
  left_join(P1 %>% rename(FIRST_SE15 = FIRST_SE, TX1DATE15 = TX1DATE, DIED15 = DIED))
#'
#' ### Incident dialysis (FIRST_SE)
#' 1. There are `r d %>% filter(FIRST_SE14 != FIRST_SE15) %>% count()` individuals who have 
#' discrepant first dialysis dates. Among them, there are `r d %>% filter(FIRST_SE15 < '2003-01-01') %>% count()` 
#' records where initial dialysis was before 2013. These are listed below:
#' 
# knitr::kable(d %>% filter(FIRST_SE15 < '2003-01-01') %>% arrange(FIRST_SE15))
#+ , echo=FALSE
pander::pander(d %>% filter(FIRST_SE15 < '2003-01-01') %>% arrange(FIRST_SE15), style = 'rmarkdown', missing = '')

#' 2. There are also 3 individuals where their dialysis date (FIRST_SE) in the 2014 database is after their death date in 
#' the 2015 database
pander::pander(d %>% filter(FIRST_SE14 > DIED15), missing='')

#' ### Withdrawal times
#+ message=F, warning=F

## First determine who is still a final discontinuation, and then compare discontinuation times
con <- dbConnect(SQLite(), file.path(dbdir, 'USRDS.sqlite3'))
con2 <- dbConnect(SQLite(), file.path(dbdir14, 'USRDS14.sqlite3'))

studyids <- dbGetQuery(con, 'select * from StudyIDs')

true_withdraw15 <- dbGetQuery(con, 'select USRDS_ID, RXGROUP from rxhist60') %>%
  right_join(studyids) %>% 
  group_by(USRDS_ID) %>% summarise(tx = paste(RXGROUP, collapse='')) %>% 
  filter(str_detect(tx,'B')) %>% 
  filter(detect_event(tx, 'B')) %>% 
  ungroup()

withdraw15_dates <- dbGetQuery(con, 'select USRDS_ID, RXGROUP, BEGDATE, ENDDATE, DEATH from rxhist60 where RXGROUP="B"') %>% 
  filter(USRDS_ID %in% studyids$USRDS_ID) %>% 
  right_join(true_withdraw15)

true_withdraw14 <- dbGetQuery(con2, 'select USRDS_ID, RXGROUP from rxhist60') %>%
  right_join(studyids) %>% 
  group_by(USRDS_ID) %>% summarise(tx = paste(RXGROUP, collapse='')) %>% 
  filter(str_detect(tx,'B')) %>% 
  filter(detect_event(tx, 'B')) %>% 
  ungroup()
withdraw14_dates <- dbGetQuery(con2, 'select USRDS_ID, RXGROUP, BEGDATE, ENDDATE from rxhist60 where RXGROUP="B"') %>% 
  filter(USRDS_ID %in% studyids$USRDS_ID) %>% 
  right_join(true_withdraw14)




indx1 <- setdiff(true_withdraw14$USRDS_ID, true_withdraw15$USRDS_ID)
indx2 <- setdiff(true_withdraw15$USRDS_ID, true_withdraw14$USRDS_ID)

explore_years <- function(id){
  con <- dbConnect(SQLite(), file.path(dbdir, 'USRDS.sqlite3'))
  con2 <- dbConnect(SQLite(), file.path(dbdir14, 'USRDS14.sqlite3'))
  query <- paste('select * from rxhist60 where USRDS_ID =', id)
  print("2014 DB")
  print(dbGetQuery(con2, query))
  print("2015 DB")
  print(dbGetQuery(con, query))
  dbDisconnect(con); dbDisconnect(con2)
}

rxhist14 <- dbGetQuery(con2, 'select USRDS_ID from rxhist60') %>% right_join(studyids)
rxhist15 <- dbGetQuery(con, 'select * from rxhist60') %>% right_join(studyids)
indx3 <- rxhist15 %>% filter(is.na(RXGROUP)) %>%  dplyr::pull(USRDS_ID) %>% intersect(withdraw14_dates$USRDS_ID)
#' There are `r length(indx1)` subjects who discontinued according to 2014 DB but not according to 2015 DB. Some of these 
#' individuals now have no record of discontinuation but die on the same date, and some restarted dialysis after 2014-06-30. 
#' There are also `r length(indx3)` subjects who are discontinuations in 2014 but whose data is missing in 2015. 
#' Conversely there are `r length(indx2)` subjects who were deemed not to have discontinued in 2014 DB but have discontinued in
#' 2015 DB. Some of these are new discontinuation status before death (which is on the same date as before), and some have 
#' discontinued in the last year. 
#' 
#' ## Transplant dates

tx_2014 <- dbGetQuery(con2, 'select USRDS_ID, TX1DATE from patients') %>% right_join(studyids)
tx_2015 <- dbGetQuery(con, 'select USRDS_ID, TX1DATE from patients') %>% right_join(studyids)

tx_discr <- tx_2014 %>% full_join(tx_2015, by = 'USRDS_ID', suffix = c('_14','_15')) %>% filter(TX1DATE_14 != TX1DATE_15) %>% 
  mutate(date_diff = as.Date(TX1DATE_15)-as.Date(TX1DATE_14))
#' There are `r nrow(tx_discr)` subjects who have different transplant dates in 2014 and 2015, and these daes can 
#' vary quite a bit. The difference in dates can range from `r min(as.numeric(tx_discr$date_diff))/365` years to 
#' `r max(as.numeric(tx_discr$date_diff))/365` years.
#'  