#!/bin/bash

echo "R tree height"
Rscript get_TreeHeight.R ../data/trees.csv

echo "Python tree height"
python3 get_TreeHeight.py ../data/trees.csv