#!/usr/bin/python

import sys
from math import *

import matplotlib
from matplotlib import pyplot as plt
from scipy import stats
import numpy as np

# read in the parameters when generating the data
data_for_plot = open("data_for_plot").readlines()

initial_N  = int(data_for_plot[0])
max_N      = int(data_for_plot[1])
N_step     = int(data_for_plot[2])
iterations = int(data_for_plot[3])
particles  = int(data_for_plot[4])

# these are the values of n we will use throughout
ns = range(initial_N, max_N+1, N_step)

# turn a time of the format 1m35.049s into a number of seconds 
def decode_time(x):
	m = float(x.split('m')[0])
	s = float(x.split('m')[1].split('s')[0])
	return 60 * m + s



# read in times and all data
times_no_opt = map(lambda x: decode_time(x.split(' ')[1]), open("times_no_opt").readlines())
times_with_opt = map(lambda x: decode_time(x.split(' ')[1]), open("times_with_opt").readlines())

data_no_opt = [0 for n in ns]
data_with_opt = [0 for n in ns]
for i in range(len(ns)):
	data_no_opt[i] = map(lambda x: float(x.split(',')[1]), open("data_no_opt_{0}".format(ns[i])).readlines())
	data_with_opt[i] = map(lambda x: float(x.split(',')[1]), open("data_with_opt_{0}".format(ns[i])).readlines())



# work out the means by averaging the data
mean_no_opt = [0 for n in ns]
mean_with_opt = [0 for n in ns]
for i in range(len(ns)):
	mean_no_opt[i] = sum(data_no_opt[i]) / (iterations * particles)
	mean_with_opt[i] = sum(data_with_opt[i]) / (iterations * particles)



# plot the times
plt.subplot(121)
plt.plot(ns, times_no_opt, color='#AA0000', label='no opt', linewidth=4)
plt.plot(ns, times_with_opt, color='#00AAAA', label='with opt', linewidth=4)
plt.xlim(initial_N, max_N)
plt.ylim(0, max(max(times_no_opt), max(times_with_opt)) + 1)
plt.title('time against n')
plt.xlabel('n')
plt.ylabel('time')
plt.legend(loc=2)

# plot the means (ignoring n=0 if it exists)
s = 0 if ns[0] != 0 else 1

plt.subplot(122)
plt.xlim(ns[s], max_N)
plt.ylim(min(min(mean_no_opt[s:]), min(mean_with_opt[s:])) - 0.01,
	     max(max(mean_no_opt[s:]), max(mean_with_opt[s:])) + 0.01)
plt.scatter(ns[s:], mean_no_opt[s:], color='#AA0000', label='no opt', linewidth=4)
plt.scatter(ns[s:], mean_with_opt[s:], color='#00AAAA', label='with opt', linewidth=4)
plt.title('mean value against n')
plt.xlabel('n')
plt.ylabel('mean value')
plt.legend(loc=2)

plt.figure()

# work out how many wide and high the grid should be
h = int(ceil(sqrt(0.5 * len(ns))))
w = int(ceil(len(ns) * 1.0 / h))

# plot hists for each dataset
for i in range(len(ns)):
	plt.subplot(h, w, i+1)
	min_val = min(data_with_opt[i])
	max_val = max(data_with_opt[i])
	plt.xlim(min_val, max_val)
	bins = [ min_val + (j / 41.0) * (max_val - min_val) for j in range(41) ]
	plt.hist(data_no_opt[i], bins=bins, color='#AA0000', alpha=0.5, normed=True, label='no opt')
	plt.hist(data_with_opt[i], bins=bins, color='#00AAAA', alpha=0.5, normed=True, label='with_opt')
	plt.title('histogram for n={0}'.format(ns[i]))
	plt.xlabel('n')
	plt.ylabel('frequency')
	plt.legend(loc=1)

matplotlib.rcParams.update({'font.size': 18})

plt.show()
