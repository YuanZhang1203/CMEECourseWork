#!/usr/bin/env bash
# Author: Yuan Zhang  yz12119@ic.ac.uk
# Script: run the miniproject
# Desc: run_miniproject.sh
# Date: March 2020

Rscript Data_preparation.R
Rscript model_fitting.R
Rscript results_plotting.R
bash compile.sh Miniproject
mv Miniproject.pdf ../Results/




