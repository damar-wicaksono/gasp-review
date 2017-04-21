#!/bin/bash
num_dims=(4 8 16 32)
for i in "${num_dims[@]}";
do
    for j in `seq 1 25`;
    do
        gsa_create_sample -n $i -d 2 -m srs -o srs_${i}_2_${j}.csv
        gsa_create_sample -n $i -d 2 -m lhs -o lhs_${i}_2_${j}.csv
        gsa_create_sample -n $i -d 2 -m sobol -rand -o sobol_${i}_2_${j}.csv
    done
done
for i in "${num_dims[@]}";
do
    for j in `seq 1 25`;
    do
        gsa_create_sample -n $i -d 2 -m lhs-opt -o lhs-opt_${i}_2_${j}.csv
    done
done
