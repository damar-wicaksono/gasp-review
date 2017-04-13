#!/bin/bash
num_smpl=(16 32 64 128)
num_dims=8
for i in "${num_smpl[@]}";
do
    for j in `seq 1 25`;
    do
        gsa_create_sample -n $i -d $num_dims -m srs -o srs_${i}_${num_dims}_${j}.csv
        gsa_create_sample -n $i -d $num_dims -m lhs -o lhs_${i}_${num_dims}_${j}.csv
        gsa_create_sample -n $i -d $num_dims -m sobol -rand -o sobol_${i}_${num_dims}_${j}.csv
    done
done
num_smpl=(16 32)
for i in "${num_smpl[@]}";
do
    for j in `seq 1 25`;
    do
        gsa_create_sample -n $i -d $num_dims -m lhs-opt -o lhs-opt_${i}_${num_dims}_${j}.csv
    done
done
