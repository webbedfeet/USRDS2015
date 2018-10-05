##%######################################################%##
#                                                          #
####    Tables and figures for hospitalization study    ####
#                                                          #
##%######################################################%##


# setup ---------------------------------------------------------------------------------------

ProjTemplate::reload()
dbdir = verifyPaths(); dir.exists(dbdir)
dropdir <- file.path(ProjTemplate::find_dropbox(), 'NIAMS','Ward','USRDS2015','data')
load(file.path(dropdir, 'modeling_data.rda'))


# Table 1 -------------------------------------------------------------------------------------

getTable1 <- function(d){
  out <- d %>% 
    mutate(age_cat = case_when(
      INC_AGE <=49 ~ '18-49',
      INC_AGE %in% seq(50,59) ~ '50-59',
      INC_AGE %in% seq(60,69) ~ '60-69',
      INC_AGE %in% seq(70,79) ~ '70-79',
      INC_AGE >= 80 ~ '80+'
    ),
    REGION = factor(REGION, levels = c('Northeast','South','Midwest','West')),
    SEX = factor(ifelse(SEX=='1','Male','Female'),levels = c('Male','Female')),
    time_on_dialysis = as.numeric(time_on_dialysis)) %>% 
    select(Race, Age = age_cat, Sex = SEX, Region = REGION, SES = zscore, 
           `Comorbidity index` = comorb_indx, 
           `Time on dialysis` = time_on_dialysis) %>% 
    tableone::CreateTableOne(data = ., strata = 'Race', 
                             test = F, 
                             vars = setdiff(names(.), c('Race')))
  out <- print(out, 
               nonnormal = c("Comorbidity index", 'SES', "Time on dialysis"))
  out <- as.data.frame(out) %>% rownames_to_column('Variable')
  return(out)
}

tab1 <- map(modeling_data[c('stroke_primary','LuCa','dement','thrive')], getTable1)
names(tab1) <- c('Stroke', 'Lung cancer', 'Dementia', "Failure to thrive")
openxlsx::write.xlsx(tab1, file = 'TableOne.xlsx', colWidths = 'auto')

