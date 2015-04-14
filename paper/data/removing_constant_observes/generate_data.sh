#!/bin/bash

iterations=1000
particles=100

./clean_data.sh

touch data_for_plot
echo $iterations >> data_for_plot
echo $particles  >> data_for_plot

../../../g2c -i prog.g -o prog_no_opt.c -O 0
../../../compile_probc prog_no_opt.c prog_no_opt
(time ./prog_no_opt -i $iterations -p $particles) 2>&1 > data_no_opt | grep real | sed "s/real\t//" > time_no_opt

../../../g2c -i prog.g -o prog_with_opt.c -O 2
../../../compile_probc prog_with_opt.c prog_with_opt
(time ./prog_with_opt -i $iterations -p $particles) 2>&1 > data_with_opt | grep real | sed "s/real\t//" > time_with_opt