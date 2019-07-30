library(fs)
library(abhiR)
dropdir <- path(find_dropbox(),'NIAMS','Ward','USRDS2015','data')
datafiles_nocause <- dir_ls(dropdir, type = 'file', regexp = 'fst|rd[sa]')
datafiles_cause <- dir_ls(path(dropdir, 'revision_JASN'), type = 'file', 
                          regexp = 'fst|rd[sa]')
commonfiles <- intersect(basename(datafiles_cause),
                         basename(datafiles_nocause))

rdsfiles <- grep('rds',commonfiles, value=T)
rdafiles <- grep('rda', commonfiles, value = T)

cause_dat <- new.env()
nocause_dat <- new.env()

for(f in rdafiles){
  load(path(dropdir, f), envir = nocause_dat)
  load(path(dropdir, 'revision_JASN', f), envir=cause_dat)
}
for(f in rdsfiles){
  objname <- stringr::str_remove(f, '.rds')
  assign(objname, readRDS(path(dropdir, f)), envir = nocause_dat)
  assign(objname, readRDS(path(dropdir, 'revision_JASN', f)), envir = cause_dat)
}
