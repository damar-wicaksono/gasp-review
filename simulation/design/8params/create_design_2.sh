#!/bin/bash
num_dims=8
num_smpl=(128)
for i in "${num_smpl[@]}";
do
    for j in `seq 1 25`;
    do
        gsa_create_sample -n $i -d $num_dims -m lhs-opt -o lhs-opt_${i}_${num_dims}_${j}.csv
    done
done
