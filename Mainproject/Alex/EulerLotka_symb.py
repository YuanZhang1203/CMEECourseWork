from sympy import *
import matplotlib.pyplot as plt
import scipy as sc

#Set up parameter values
Temps = sc.linspace(0, 40, 100)
EA = 1.7
ED = 4.5
Tpk = 26.


# font = {'family' : 'serif',
        # 'color'  : 'darkred',
        # 'weight' : 'bold',
        # 'size'   : 16,
        # }
  
############# Symbolic analysis: Euler-Lotka ################

# Define symbols:

r, k, z, zbar, x, a, bpk, xpk, pEA, Tpk, b0, EA, ED, K, T, b0_b, b0_p, b0_a, b0_z = symbols('r k z zbar x a bpk xpk pEA Tpk b0 EA ED K T b0_b b0_p b0_a b0_z')

# EuLot = simplify(exp(-r * x) * exp(-(zbar*a + z*(x - a))) * bpk * exp(k - 1 - ((x-a)/((xpk - a)/(k - 1)))) * (x-a)**(k-1) * (xpk - a)**(1-k))

# integrate(EuLot, (x, a, oo))

M_Parham = bpk * pEA / (a * z **2)

bpk_T = b0_b * exp(-EA/(K * T)) / (1 + (EA / (ED - EA)) + exp((ED / K)* ((1/Tpk) - (1/T))))
pEA_T = b0_p * exp(-EA/(K * T)) / (1 + (EA / (ED - EA)) + exp((ED / K)* ((1/Tpk) - (1/T))))
a_T = b0_a * exp(-EA/(K * T)) / (1 + (EA / (ED - EA)) + exp((ED / K)* ((1/Tpk) - (1/T))))
z_T = b0_z * exp(-EA/(K * T)) / (1 + (EA / (ED - EA)) + exp((ED / K)* ((1/Tpk) - (1/T))))

sens_a1 = simplify(diff(M_Parham, a))
sens_a2 = simplify(diff(a_T, T))

sens_z = simplify(diff(M_Parham, z))
sens_pEA = simplify(diff(M_Parham, pEA))
sens_bpk = simplify(diff(M_Parham, bpk))

# print(pretty(EuLot,use_unicode = False))
