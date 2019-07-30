#'
#+ echo = F, results = 'asis'
## Parsing the USRDS formats using pdftools
## 

library(pdftools)
library(fs)
library(tidyverse)
library(tableone)
source('extract_fmt.R')
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


# Create ESRD cause codebook ----------------------------------------------

disgrpc_code <- filter(data_formats, Variable == 'DISGRPC') %>% 
  select(Format, Description)
saveRDS(disgrpc_code, path(dropdir, 'disgrpc_code.rds'))
