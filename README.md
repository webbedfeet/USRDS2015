# USRDS 2015

## Background
This is a refactoring of the original USRDS project that will utilize 

+ 2015 USRDS data
+ 2015 USRDS hospitalization data

The purpose of the refactoring is to make the analysis more efficient, given the 
large-ish nature of the dataset

All data sets are stored in both a [MonetDB](http://www.monetdb.org) and [SQLite](https://sqlite.org) database, to speed up basic queries, selects and merges, and to 
generate descriptive statistics quickly. 