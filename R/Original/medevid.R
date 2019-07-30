# Extract comorbidities data from medevid

ProjTemplate::reload()
dbdir <- verifyPaths()
if(is.na(dbdir)){
  stop('Disk not available')
}
con <- dbConnect(SQLite(), file.path(dbdir, 'USRDS.sqlite3') )

dbListTables(con)
sort(dbListFields(con, 'medevid'))

medevid = tbl(con, 'medevid')
medevid


dbDisconnect(con)
