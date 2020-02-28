#!/usr/bin/python
import numpy as np
import pandas as pd
import os
if not os.path.exists('R_local'):
  os.makedirs('R_local')

nsim = 1000

seeds = np.array(pd.read_csv('seeds.txt')).ravel()


with open('simulation_template_local.R','r') as f:
  template = f.readlines()

for age_grp in list(range(1,7)):
  for iter_no in list(range(1, nsim+1)):
    # print(iter_no)
    y = [u.replace('{agegrp_no}',str(age_grp)) for u in template]
    y = [u.replace('{iter_no}', str(iter_no)) for u in y]
    y = [u.replace('{{i}}', '{i}') for u in y]
    y = [u.replace('{{j}}', '{j}') for u in y]
    y = [u.replace('{seed}',str(seeds[iter_no-1])) for u in y]
    fname = 'R_local/script'+str(age_grp)+'_'+str(iter_no)+'.R'
    with open(fname, 'w') as f:
      f.writelines(y)

# with open('R/swarmfile','w') as f:
#   for agegrp in list(range(1, 7)):
#     for iter_no in list(range(1, nsim+1)):
#       fname = 'script'+str(agegrp)+'_'+str(iter_no)+'.R'
#       bl = 'source('+'"'+fname+'"'+')'
#       f.write("Rscript -e '"+bl+"'\n")

