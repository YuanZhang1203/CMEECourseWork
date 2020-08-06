#!usr/bin/env python
#################################################
# Title: 4 rm plot in python
# MSc CMEE 
# July 2020 
# Author: YUAN ZHANG 
# Refer to: VecMismatch_SI.ipynb
#################################################

#Load some modules etc
%matplotlib inline # A particularly interesting backend, provided by IPython, is the inline backend. This is available only for the Jupyter Notebook and the Jupyter QtConsole.
import matplotlib.pyplot as plt
from sympy import *
import scipy as sc
import numpy as np
init_printing()

#### 1. Model for temperature-dependent fitness of disease vectors ##########
#1) Euler-Lotka equation:
x, l_a, b_pk, alpha, z, z_J, kappa, T, M_0, K, t = var('x l_a b_pk alpha z z_J kappa T M_0 K t',real=True, positive = true) #assign symbolic variables
r_m  = var('r_m', real = True) # r_m can be negative