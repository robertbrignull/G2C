#!/usr/bin/python

import sys
import random

if len(sys.argv) < 2:
	print "Usage: generate_source_file v"
	print "where v is the value of (poisson 1) being observed (must be a non-negative integer)"
	exit(1)

try:
	v = int(sys.argv[1])
except ValueError:
	print "v must be an integer"
	exit(1)

if v < 0:
	print "v must be a non-negative integer"
	exit(1)

source_file = open("prog_{0}.g".format(v), "w")



##### Start outputting G code #####

source_file.write("[assume x (normal 0 1)]\n")
source_file.write("[assume y (poisson 1)]\n")
source_file.write("[observe (normal y 0.1) {0}]\n".format(v))
source_file.write("[predict x]\n")

##### End outputting G code #####



source_file.close()
