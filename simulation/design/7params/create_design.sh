#!/bin/bash
num_dims=(16 32 64 128)
for i in "${num_dims[@]}";
do
    for j in `seq 1 25`;
    do
        gsa_create_sample -n $i -d 7 -m srs -o srs_${i}_7_${j}.csv
        gsa_create_sample -n $i -d 7 -m lhs -o lhs_${i}_7_${j}.csv
        gsa_create_sample -n $i -d 7 -m sobol -rand -o sobol_${i}_7_${j}.csv
    done
done
for i in "${num_dims[@]}";
do
    for j in `seq 1 25`;
    do
        gsa_create_sample -n $i -d 7 -m lhs-opt -o lhs-opt_${i}_7_${j}.csv
    done
done
