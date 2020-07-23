#!/usr/bin/env bash
# Author: Yuan Zhang  yz12119@ic.ac.uk
# Script: compile.sh
# Desc: Compiles LaTeX files
# Date: April 2020


pdflatex $1.tex
pdflatex $1.tex
bibtex $1
pdflatex $1.tex
pdflatex $1.tex
evince $1.pdf &

## Cleanup
rm *~
rm *.aux
rm *.dvi
rm *.log
rm *.nav
rm *.out
rm *.snm
rm *.toc
rm *.blg
rm *.bbl



