#!/bin/bash

initial_V=3
max_V=10
V_step=7

iterations=1000
particles=100

./clean_data.sh

touch data_for_plot
echo $initial_V  >> data_for_plot
echo $max_V      >> data_for_plot
echo $V_step     >> data_for_plot
echo $iterations >> data_for_plot
echo $particles  >> data_for_plot

for i in `seq $initial_V $V_step $max_V`;
do
	./generate_source_file.py $i

	../../../g2c -i prog_$i.g -o prog_no_opt_$i.c -O 0
	../../../compile_probc prog_no_opt_$i.c prog_no_opt_$i
	./prog_no_opt_$i -i $iterations -p $particles > data_no_opt_$i
	
	../../../g2c -i prog_$i.g -o prog_with_opt_$i.c -O 2
	../../../compile_probc prog_with_opt_$i.c prog_with_opt_$i
	./prog_with_opt_$i -i $iterations -p $particles > data_with_opt_$i
done
