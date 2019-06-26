#'
#+ echo = F, results = 'asis'
## Parsing the USRDS formats using pdftools
## 

library(pdftools)
library(fs)
library(tidyverse)
library(tableone)
drop_dir <- path(ProjTemplate::find_dropbox(), 'NIAMS','Ward','USRDS2015')

## Grab the Research Guide files
downloads <- path(drop_dir,'docs')
pdffiles <- dir_ls(downloads, regex = 'USRDS.*pdf$')

## Extract data from PDF 

formats <- pdffiles[2]
txt2 <- pdf_text(formats)
data_formats <- map(txt2[-(1:2)], extract_fmt) %>% 
  Reduce(bind_rows, .) %>% 
  mutate_at(vars(Variable, Type), ~na_if(., '')) %>% 
  fill(., Variable, Type)

## Create hospitalization datasets

hospitalization <- readRDS(path(drop_dir,'data/hospitalization_ids.rds'))
raw <- readRDS(path(drop_dir, 'data/rawdata.rds'))
hosp_data <- map(hospitalization, ~left_join(., raw))
disgrpc_code <- filter(data_formats, Variable == 'DISGRPC') %>% 
  select(Format, Description)
hosp_data <- hosp_data %>% 
  map( ~left_join(., disgrpc_code, by=c('DISGRPC'='Format'))) %>% 
  map(~mutate(., Description = fct_other(Description,keep=c('Diabetes','Hypertension','Glomeruloneph.')))) %>% 
  map(~mutate(., Description = ifelse(DISGRPC %in% c('**OTHER**','8'), NA, Description)))

out <- hosp_data %>% map(~kableone(CreateCatTable('Description', data = .)))

sink('cause_esrd.md')
for(n in names(out)){
  cat(paste('#', n))
  print(out[[n]])
  cat('\n')
}
sink()


# pdis_codes = Reduce(c, dat)
# pdis_codes <- pdis_codes %>% str_trim() %>% str_split_fixed('[ ]+', 2) %>% 
#   as.data.frame(stringsAsFactors = F) %>% set_names(c('Code','Description')) %>% 
#   mutate(Description = str_to_lower(Description)) %>% 
#   mutate(Code_stripped = str_remove(Code, '[A-Za-z]+'))

# analytic <- readRDS(path(drop_dir, 'data/Analytic.rds'))
# analytic$PDIS <- str_remove(analytic$PDIS,'[A-Za-z]+')
# 
# codes <- list(
# 'diabetes' =  filter(pdis_codes,str_detect(Description,'diabetes')),
# 'hypertension' = filter(pdis_codes,str_detect(Description,'hypertension')),
# 'gn' = filter(pdis_codes,str_detect(Description,'\\bgn\\b|glumerolunephritis'))
# ) 
# 
# codes %>% 
#   map(~select(., Code, Description)) %>% 
#   openxlsx::write.xlsx(.,'PDIS_codes.xlsx')
# 

# analytic_pdis <- left_join(analytic, pdis_codes, by = c('PDIS' = 'Code_stripped'))
# analytic_pdis %<>% 
#   mutate(cause_esrd = case_when(PDIS %in% codes$hypertension$Code_stripped~'hypertension',
#                                 PDIS %in% codes$diabetes$Code_stripped~ 'diabetes',
#                                 PDIS %in% codes$gn$Code_stripped ~ 'gn',
#                                 TRUE ~ 'other'))
# 
# library(tableone)
# CreateCatTable(vars = c('Outcomes','Cause of ESRD'), data = tmp) %>% 
#   kableone(format = 'markdown')
# 
# 
# 
ids = readRDS('data/hospitalization_ids.rds')

hospitalization_data <- ids %>% map(~analytic %>% filter(USRDS_ID %in% .))
