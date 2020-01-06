#!/bin/bash
#Author: yz12119@ic.ac.uk
#Script: run_cluster.sh
#Desc: running scripts on the cluster
#PBS -l walltime=12:00:00
#PBS -l select=1:ncpus=1:mem=1gb
module load anaconda3/personal
cp $HOME/yuan_HPC_2019_cluster.R $TMPDIR
echo "R is about to run"
R --vanilla < $HOME/yuan_HPC_2019_cluster.R
mv cluster_run_* $HOME/results
echo "R has finished running"

