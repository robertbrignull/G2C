#!/usr/bin/python

import sys
import random

if len(sys.argv) < 2:
	print "Usage: generate_source_file n"
	print "where n is the number of observes to be merged"
	exit(1)

try:
	n = int(sys.argv[1])
except ValueError:
	print "n must be an integer"
	exit(1)

if n < 1:
	print "n must be a non-negative integer"
	exit(1)

source_file = open("prog_{0}.g".format(n), "w")



##### Start outputting G code #####

source_file.write("[assume m (normal 5 1)]\n")

source_file.write("[assume f (lambda () -> Num m)]\n")

for i in xrange(n):
	source_file.write("[observe (normal (f) 0.1) 4]\n")

source_file.write("[predict m]\n")

##### End outputting G code #####



source_file.close()
