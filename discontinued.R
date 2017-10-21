# Script to interrogate IDs among those who discontinued dialysis to see if 
# the propensity to discontinue depends on race and prevalent comorbidities
# 
# We're not doing incident comorbidities today (10/19/17) since we're not sure 
# if prevalence at study entry is available in medevid. To me it appears not for most
# of the comorbidities
# 
# The IDs with prevalent comorbidities is interrogated in hospitalization.R and
# stored in data/hospitalization_idss.rds as a list by comorbidity.

source('lib/reload.R'); reload()
dbdir15 <- verifyPaths()
dbdir14 <- verifyPaths(year=2014)

con14 <- dbConnect(SQLite(), file.path(dbdir14, 'PR_db'))
discontinue_id <- dbGetQuery(con14, 'select USRDS_ID from AnalyticData where cens_type=3')

