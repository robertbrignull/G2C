#!/bin/bash

initial_N=0
max_N=14
N_step=2

iterations=1000
particles=100

./clean_data.sh

touch data_for_plot
echo $initial_N  >> data_for_plot
echo $max_N      >> data_for_plot
echo $N_step     >> data_for_plot
echo $iterations >> data_for_plot
echo $particles  >> data_for_plot

for i in `seq $initial_N $N_step $max_N`;
do
	./generate_source_file.py $i

	../../../g2c -i prog_$i.g -o prog_no_opt_$i.c -O 0
	../../../compile_probc prog_no_opt_$i.c prog_no_opt_$i
	(time ./prog_no_opt_$i -i $iterations -p $particles) 2>&1 > data_no_opt_$i | grep real | sed "s/real	/$i /" >> times_no_opt
	
	../../../g2c -i prog_$i.g -o prog_with_opt_$i.c -O 2
	../../../compile_probc prog_with_opt_$i.c prog_with_opt_$i
	(time ./prog_with_opt_$i -i $iterations -p $particles) 2>&1 > data_with_opt_$i | grep real | sed "s/real	/$i /" >> times_with_opt
done
