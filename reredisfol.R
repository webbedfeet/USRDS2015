# Looking at reasons for discontinuation
datadir2014 <- "P:/Mohammed"
source('lib/reload.R'); reload()
dbdir <- verifyPaths() # is the database directories available
if (is.na(dbdir)) stop('Databases not accessible')
sql_conn <- src_sqlite(file.path(dbdir,'USRDS.sqlite3'))

# con1 = dbConnect(MonetDBLite(), dbdir)
# mdb_conn <- src_monetdblite(dbdir)
# # conn1 <- dbConnect(MonetDBLite(), dbdir)

deaths_db = tbl(sql_conn, 'death')
deaths_db %>% count(REREDISFOL) -> discontinue_summary

bl <- deaths_db %>% select(USRDS_ID, REREDISFOL) %>% collect()
ids = read_csv('data/raw/ids2014.csv')
bl <- bl %>% filter(USRDS_ID %in% ids$USRDS_ID)
summ <- bl %>% count(REREDISFOL)
summ[1,1] = 'Unavailable'
x <-  tibble(REREDISFOL = 'Unavailable-not in DEATH', n = nrow(ids)-nrow(bl))
bl <- rbind(summ, x) %>% arrange(REREDISFOL)

bl <- bl %>% mutate(Percent = round(n/sum(n)*100,2), 
                    "Cumulative percent" = round(cumsum(n)/sum(n)*100,2))

