#!/usr/bin/python

import sys
from math import *

# read in the parameters when generating the data
data_for_plot = open("data_for_plot").readlines()

iterations = int(data_for_plot[0])
particles  = int(data_for_plot[1])

# turn a time of the format 1m35.049s into a number of seconds 
def decode_time(x):
	m = float(x.split('m')[0])
	s = float(x.split('m')[1].split('s')[0])
	return 60 * m + s



# read in times and all data
time_no_opt = decode_time(open("time_no_opt").read())
time_with_opt = decode_time(open("time_with_opt").read())

data_no_opt = map(lambda x: float(x.split(',')[1]), open("data_no_opt").readlines())
data_with_opt = map(lambda x: float(x.split(',')[1]), open("data_with_opt").readlines())



# count the fraction of unique particles
frac_unique_no_opt = 0
frac_unique_with_opt = 0
for i in range(iterations):
	frac_unique_no_opt += len(set(data_no_opt[(i*particles):((i+1)*particles)])) * 1.0 / particles
	frac_unique_with_opt += len(set(data_with_opt[(i*particles):((i+1)*particles)])) * 1.0 / particles

frac_unique_no_opt /= iterations
frac_unique_with_opt /= iterations



# print out how much faster the optimized version was
print "The optimized version was {0}% faster".format((time_no_opt - time_with_opt) / time_no_opt * 100)

# print out how many unique particles there were
print "The non-optimized version had {0}% unique particles".format(frac_unique_no_opt * 100.0)
print "The optimized version had {0}% unique particles".format(frac_unique_with_opt * 100.0)
