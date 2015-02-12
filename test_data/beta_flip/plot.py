#!/usr/bin/python

import sys

from matplotlib import pyplot as plt
from scipy import stats
import numpy as np

data_no_opt = map(lambda x: float(x.split(',')[1]), open('data_no_opt').readlines())
data_with_obs = map(lambda x: float(x.split(',')[1]), open('data_with_obs').readlines())
data_without_obs = map(lambda x: float(x.split(',')[1]), open('data_without_obs').readlines())

plt.xlim(0.7, 1.0)
x = np.linspace(0.7, 1, 50)

plt.hist(data_no_opt, bins=x, histtype="stepfilled", normed=True, color='#AA0000', alpha=0.5)
plt.hist(data_with_obs, bins=x, histtype="stepfilled", normed=True, color='#00AA00', alpha=0.5)
plt.hist(data_without_obs, bins=x, histtype="stepfilled", normed=True, color='#0000AA', alpha=0.5)

plt.show()
