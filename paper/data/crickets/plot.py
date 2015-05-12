#!/usr/bin/python

import sys

import matplotlib
from matplotlib import pyplot as plt

data_no_opt = map(lambda x: float(x.split(',')[1]), open("data_no_opt").readlines())
data_mid_opt = map(lambda x: float(x.split(',')[1]), open("data_mid_opt").readlines())
data_full_opt = map(lambda x: float(x.split(',')[1]), open("data_full_opt").readlines())

plt.subplot(131)
plt.hist(data_no_opt, bins=50, histtype="stepfilled", normed=True)
plt.title('histogram when not optimized')
plt.xlabel('gradient value')
plt.ylabel('frequency')
plt.legend(loc=2)

plt.subplot(132)
plt.hist(data_mid_opt, bins=50, histtype="stepfilled", normed=True)
plt.title('histogram when mid optimizations')
plt.xlabel('gradient value')
plt.ylabel('frequency')
plt.legend(loc=2)

plt.subplot(133)
plt.hist(data_full_opt, bins=50, histtype="stepfilled", normed=True)
plt.title('histogram when fully optimized')
plt.xlabel('gradient value')
plt.ylabel('frequency')
plt.legend(loc=2)

matplotlib.rcParams.update({'font.size': 18})

plt.show()
