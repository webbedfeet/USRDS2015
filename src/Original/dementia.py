#!/Users/abhijit/anaconda/bin/python
# Author: Abhijit Dasgupta
# This script queries the USRDS2015 SQLite database to obtain IDs from
# hospitalization records for incident comorbidities

# Preamble
import sqlite3
import os.path
import re
import os

os.chdir(os.path.expanduser('~/NIAMS/Ward/USRDS2015'))
dbPath = os.path.join('/Volumes','ARAASTAT','NIAMS','Ward','USRDS',
    'Data','2015 data', 'USRDS.sqlite3')

con = sqlite3.connect(dbPath)
c = con.cursor()

# Dementia
# Here we choose all ids for whom one of the diagnoses is dementia. It need not
# be the primary diagnosis

## till2009 extraction

print("Starting 2009 extraction ...\n")
dement_2009 = []


c.execute('SELECT `USRDS_ID` AS `USRDS_ID`, `HSDIAG1` AS `HSDIAG1`, `HSDIAG2` AS `HSDIAG2`, `HSDIAG3` AS `HSDIAG3`, `HSDIAG4` AS `HSDIAG4`, `HSDIAG5` AS `HSDIAG5`, `HSDIAG6` AS `HSDIAG6`, `HSDIAG7` AS `HSDIAG7`, `HSDIAG8` AS `HSDIAG8`, `HSDIAG9` AS `HSDIAG9`, `HSDIAG10` AS `HSDIAG10` FROM `till2009`')
for row in c:
    if any([re.search('^290', x) for x in row[1:]]):
        dement_2009.append(row[0])
        continue
    if any([re.search('^2941', x) for x in row[1:]]):
        dement_2009.append(row[0])
        continue
    if any([re.search('^3310', x) for x in row[1:]]):
        dement_2009.append(row[0])
        continue
    if any([re.search('^3311',x) for x in row[1:]]):
        dement_2009.append(row[0])
        continue
    if any([re.search('^3312', x) for x in row[1:]]):
        dement_2009.append(row[0])
        continue


## from 2010 extraction
print("Starting 2010 extraction ...\n")
dement_2010 = []
c.execute('SELECT `USRDS_ID`,`HSDIAG1` AS `HSDIAG1`, `HSDIAG2` AS `HSDIAG2`, `HSDIAG3` AS `HSDIAG3`, `HSDIAG4` AS `HSDIAG4`, `HSDIAG5` AS `HSDIAG5`, `HSDIAG6` AS `HSDIAG6`, `HSDIAG7` AS `HSDIAG7`, `HSDIAG8` AS `HSDIAG8`, `HSDIAG9` AS `HSDIAG9`, `HSDIAG10` AS `HSDIAG10`, `HSDIAG11` AS `HSDIAG11`, `HSDIAG12` AS `HSDIAG12`, `HSDIAG13` AS `HSDIAG13`, `HSDIAG14` AS `HSDIAG14`, `HSDIAG15` AS `HSDIAG15`, `HSDIAG16` AS `HSDIAG16`, `HSDIAG17` AS `HSDIAG17`, `HSDIAG18` AS `HSDIAG18`, `HSDIAG19` AS `HSDIAG19`, `HSDIAG20` AS `HSDIAG20`, `HSDIAG21` AS `HSDIAG21`, `HSDIAG22` AS `HSDIAG22`, `HSDIAG23` AS `HSDIAG23`, `HSDIAG24` AS `HSDIAG24`, `HSDIAG25` AS `HSDIAG25`, `HSDIAG26` AS `HSDIAG26` FROM `from2010` ')
for row in c:
    if any([re.search('^290', x) for x in row[1:]]):
        dement_2010.append(row[0])
        continue
    if any([re.search('^2941', x) for x in row[1:]]):
        dement_2010.append(row[0])
        continue
    if any([re.search('^3310', x) for x in row[1:]]):
        dement_2010.append(row[0])
        continue
    if any([re.search('^3311',x) for x in row[1:]]):
        dement_2010.append(row[0])
        continue
    if any([re.search('^3312', x) for x in row[1:]]):
        dement_2010.append(row[0])
        continue

con.close()

from more_itertools import unique_everseen

out = list(unique_everseen(dement_2009+dement_2010))

with open('data/Dementia.csv','w') as f:
    for x in out :
        f.write(str(int(x))+'\n')
