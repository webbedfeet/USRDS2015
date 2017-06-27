# Script to verify if there is current access to database directories and to 
# identify the directories where the databases reside

if (Sys.info()['sysname'] == 'Windows') { # Work windows system
  a <- system('wmic volume get name,label', intern = T)
  a <- a[-length(a)] # remove empty string
  labs <- unlist(str_split(a[[1]], '\\s{2,}'))
  bl <- do.call('rbind',
                lapply(a[-1], str_split, '\\s{2,}') %>% lapply(unlist))
  bl <- data.frame(bl, stringsAsFactors = F)
  names(bl) <- labs
  if (!any(c('ARAASTAT','My Book') %in% bl$Label)) {
    stop('No access to databases')
  }
  # Priority is for My Book, then ARAASTAT
  if ('My Book' %in% bl$Label) {
    dbdir <- file.path(bl$Name[bl$Label == 'My Book'], 'Data','USRDS','2015 data')
  } else {
    dbdir <- file.path(bl$Name[bl$Label == 'ARAASTAT'],'NIAMS','Ward','USRDS','Data','2015 data')
  }
  
}