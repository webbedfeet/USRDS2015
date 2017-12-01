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
con2 <- dbConnect(SQLite(), file.path(dbdir14, 'PR_db'))
knitr::opts_chunk$set(echo=FALSE)

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

con <- dbConnect(SQLite(), file.path(dbdir, 'USRDS.sqlite3'))

#' 
