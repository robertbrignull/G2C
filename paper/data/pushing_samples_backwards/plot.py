#!/usr/bin/python

import sys
from math import *

import matplotlib
from matplotlib import pyplot as plt

# read in the parameters when generating the data
data_for_plot = open("data_for_plot").readlines()

initial_V  = int(data_for_plot[0])
max_V      = int(data_for_plot[1])
V_step     = int(data_for_plot[2])
iterations = int(data_for_plot[3])
particles  = int(data_for_plot[4])

# these are the values of n we will use throughout
vs = range(initial_V, max_V+1, V_step)



# read in all data
data_no_opt = [0 for v in vs]
data_with_opt = [0 for v in vs]
for i in range(len(vs)):
	data_no_opt[i] = map(lambda x: float(x.split(',')[1]), open("data_no_opt_{0}".format(vs[i])).readlines())
	data_with_opt[i] = map(lambda x: float(x.split(',')[1]), open("data_with_opt_{0}".format(vs[i])).readlines())

# work out running means
running_mean_no_opt = [[sum(data_set[:(i * particles)]) * 1. / (i * particles) for i in range(1, iterations+1)] for data_set in data_no_opt]
running_mean_with_opt = [[sum(data_set[:(i * particles)]) * 1. / (i * particles) for i in range(1, iterations+1)] for data_set in data_with_opt]



# plot hists for each dataset
for i in range(len(vs)):
	plt.subplot(121)
	min_val = min(data_with_opt[i])
	max_val = max(data_with_opt[i])
	plt.xlim(min_val, max_val)
	bins = [ min_val + (j / 41.0) * (max_val - min_val) for j in range(41) ]
	plt.hist(data_no_opt[i], bins=bins, color='#AA0000', alpha=0.5, normed=True)
	plt.hist(data_with_opt[i], bins=bins, color='#00AAAA', alpha=0.5, normed=True)
	plt.title('histogram for v={0}'.format(vs[i]))
	plt.xlabel('predicted value of x')
	plt.ylabel('frequency')

	plt.subplot(122)
	plt.xlim(1, iterations)
	plt.ylim(min(min(running_mean_no_opt[i][(iterations/20):]), min(running_mean_with_opt[i][(iterations/20):])) - 0.01,
		     max(max(running_mean_no_opt[i][(iterations/20):]), max(running_mean_with_opt[i][(iterations/20):])) + 0.01)
	plt.plot(range(1, iterations+1), running_mean_no_opt[i], color='#AA0000', label='no opt', linewidth=2)
	plt.plot(range(1, iterations+1), running_mean_with_opt[i], color='#00AAAA', label='with opt', linewidth=2)
	plt.title('running mean for v={0}'.format(vs[-1]))
	plt.xlabel('iteration')
	plt.ylabel('mean')
	plt.legend(loc=1)

	if i != len(vs) - 1:
		plt.figure()

matplotlib.rcParams.update({'font.size': 18})

plt.show()
