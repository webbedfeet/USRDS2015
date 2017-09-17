# Data ingestion and munging

source('lib/reload.R'); reload()
dbdir <- verifyPaths() # is the database directories available
if (is.na(dbdir)) stop('Databases not accessible')

con1 = dbConnect(MonetDBLite(), dbdir)
mdb_conn <- src_monetdblite(dbdir)
# conn1 <- dbConnect(MonetDBLite(), dbdir)

sql_conn <- src_sqlite(file.path(dbdir,'USRDS.sqlite3'))

P <- tbl(sql_conn, 'patients')
M <- tbl(sql_conn, 'medevid')

dbGetQuery(sql_conn$con, 'select count(*) from medevid')
