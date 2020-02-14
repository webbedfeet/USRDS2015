#!/usr/bin/python
import numpy as np
import os
if not os.path.exists('R_local'):
  os.makedirs('R_local')

nsim = 1000
np.random.seed(None)
np.random.seed(20845)
seeds = np.random.randint(1, 100000, size = nsim)
seeds.tofile('seeds.txt', sep = '\n', format = '%5d')
