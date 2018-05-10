#' ---
#' title: Analysis of discontinuation by race
#' author: Abhijit Dasgupta
#' date: October 22, 2017
#' ---
#' 
#+ echo=FALSE, message=FALSE, warning=FALSE, results='hide'
# Script to interrogate IDs among those who discontinued dialysis to see if 
# the propensity to discontinue depends on race and prevalent comorbidities
# 
# We're not doing incident comorbidities today (10/19/17) since we're not sure 
# if prevalence at study entry is available in medevid. To me it appears not for most
# of the comorbidities
# 
# The IDs with prevalent comorbidities is interrogated in hospitalization.R and
# stored in data/hospitalization_idss.rds as a list by comorbidity.
#+ code, echo=F, message=F, warning=F
ProjTemplate::reload()
dbdir15 <- verifyPaths()

con15 <- dbConnect(SQLite(), file.path(dbdir15, 'USRDS.sqlite3'))
dat <- tbl(con15, 'AnalyticData')
discontinue_id <- dat %>% filter(cens_type==3) %>% 
  select(USRDS_ID, RACE2, HispGrps) %>% collect() %>% 
  mutate(RACE2 = str_replace(RACE2, 'Nonhispanic ',''))

hosp_id <- readRDS(file.path(dbdir15,'hospitalization_ids.rds'))

disc_hosp_id <- lapply(hosp_id, filter, USRDS_ID %in% discontinue_id$USRDS_ID)
# sapply(disc_hosp_id, nrow)
disc_hosp_race <- lapply(hosp_id, 
                         function(x) discontinue_id %>% 
                           filter(USRDS_ID %in% x$USRDS_ID) %>% count(RACE2))
withdraw_surv <- dat  %>% 
  select(USRDS_ID,INC_AGE, SEX, RACE2, BMI, HispGrps, cens_type, surv_time, withdraw,surv_time) %>% 
  collect() %>% 
  mutate(RACE2 = str_replace(RACE2, 'Nonhispanic ', '')) %>% 
  mutate(RACE2 = factor(RACE2, 
                        levels = c('White','Black','Hispanic','Native American','Asian')))
#' In this study we will look at the risk (hazard) of discontinuing dialysis to
#' see if this risk differs by race. We treat both death and transplants as censoring
#' events. 
#' 
#' The following table shows the results of a Cox regression of discontinuation
#' by race, adjusting for age, sex and BMI of subjects. 
#+ results='asis', echo=F
coxph(Surv(surv_time, withdraw)~SEX+BMI + INC_AGE+RACE2, data = withdraw_surv) %>% 
  tidy() %>% 
  filter(str_detect(term,'RACE')) %>% 
  select(term:estimate, conf.low:conf.high) %>% 
  mutate_at(vars(estimate:conf.high), exp) %>% 
  mutate(term = str_replace(term, 'RACE2','')) %>% 
  rename(Race = term) %>% 
  knitr::kable(digits=3)

#' We find that, compared to whites, all the other race groups have significantly
#' reduced hazard of discontinuing dialysis. This can also be seen in the following
#' Kaplan-Meier analysis of discontinuation by race
#+ echo = F, fig.width=4
library(survminer)
sfit <- survfit(Surv(surv_time, withdraw)~RACE2, data=withdraw_surv)
plt <- ggsurvplot(sfit, data=withdraw_surv, risk.table=F, conf.int=F,
           legend='bottom',legend.title='', censor=F, pval=T, 
           xlab = 'Time (years)', ylab = 'Time to discontinuation')
print(plt$plot + guides(color = guide_legend(nrow = 2, byrow = T)))
#' 
#' We also looked at how the relationship between race and risk of discontinuing 
#' dialysis behaves within patients with some __prevalent__ comorbidities. 
#' 
#' ----
#' 
#' > Note to Mike: The `medevid` file doesn't give information about all the 
#' > comorbidities we are looking at. It only gives a binary indicator of presence 
#' > of the following comorbidities: 
#+ echo=F
x <- dbListFields(con15, 'medevid')
x[str_detect(x, 'COMO_')]
#'
#' -----
#+ echo=F, results='asis'
lapply(hosp_id, function(x){
  withdraw_surv %>% filter(USRDS_ID %in% x$USRDS_ID) %>% 
    coxph(Surv(surv_time, withdraw)~ INC_AGE+SEX+RACE2+BMI, data = .) %>% 
    tidy() %>% 
    filter(str_detect(term,'RACE')) %>% 
    select(term:estimate, conf.low:conf.high) %>% 
    mutate_at(vars(estimate:conf.high), exp) %>% 
    mutate(term = str_replace(term, 'RACE2','')) %>% 
    rename(Race = term)
}) %>% bind_rows(.id = 'Comorbidities') %>% 
  mutate(Comorbidities = clean_descr_col(Comorbidities)) %>% 
  knitr::kable(digits=3)

#'
#+ echo=FALSE
dbDisconnect(con15)
