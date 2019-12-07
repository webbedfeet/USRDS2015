##%######################################################%##
#                                                          #
####          Tables and Figures for Revision           ####
#                                                          #
##%######################################################%##



# Setup -------------------------------------------------------------------

abhiR::reload()
library(here)
dropdir <- here('data')
# dropdir <- path(find_dropbox(), 'NIAMS','Ward','USRDS2015','data')

analytic <- read_fst(path(dropdir, 'Revision','AnalyticUpdated.fst'))

analytic_filt <- analytic %>%
  mutate(RACE2 = factor(RACE2)) %>%
  mutate(RACE2 = fct_relevel(RACE2, 'White','Black','Asian','Native American','Hispanic')) %>%
  filter(RACE2 != 'Other') %>%
  mutate(RACE2 = fct_drop(RACE2, 'Other'))


analytic <- analytic %>%
  mutate(SEX = ifelse(SEX=='1', 'Male','Female')) %>%
  mutate_at(vars(DIABETES, Cancer,Cardia, Cva, Ihd, Pulmon, Pvasc), as.factor) %>%
  set_variable_labels(DIABETES = 'Diabetes',
                      SEX = 'Gender',
                      INC_AGE = 'Age',
                      zscore = 'Socio-economic score',
                      REGION = 'Region',
                      ESRD_Cause = 'Cause of ESRD',
                      comorb_indx = 'Comorbidity index') %>%
  set_value_labels(ESRD_Cause = c('Glomeruloneph.' = 'Glomerulonephritis'))

# Table 1 -----------------------------------------------------------------

library(tableone)

variables <- c('SEX','INC_AGE','ESRD_Cause','zscore','REGION','DIABETES',
               'Cancer','Cardia','Cva','Hyper','Ihd','Pulmon','Pvasc',
               'Smoke','BMI2','ALCOH','DRUG')

c1 <- CreateTableOne(data=analytic,
               vars = variables,
               strata = 'RACE2',
               test = F)
print(c1, format='fp', showAllLevels = T, varLabels = T  )
