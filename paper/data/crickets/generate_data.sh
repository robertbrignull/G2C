#!/bin/bash

iterations=2000
particles=100

./clean_data.sh

touch data_for_plot
echo $iterations >> data_for_plot
echo $particles  >> data_for_plot

../../../compile_probc prog_no_opt.c prog_no_opt
(time ./prog_no_opt -i $iterations -p $particles) 2>&1 > data_no_opt | grep real | sed "s/real	/$i /" >> times

../../../compile_probc prog_mid_opt.c prog_mid_opt
(time ./prog_mid_opt -i $iterations -p $particles) 2>&1 > data_mid_opt | grep real | sed "s/real	/$i /" >> times

../../../compile_probc prog_full_opt.c prog_full_opt
(time ./prog_full_opt -i $iterations -p $particles) 2>&1 > data_full_opt | grep real | sed "s/real	/$i /" >> times
