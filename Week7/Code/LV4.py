import matplotlib.pylab as p
import scipy.integrate as integrate
import scipy as sc
from scipy import stats
import sys


def dCR_dt(pops, E=0):
	R = pops[0]
	C = pops[1]
	K = 2
	dRdt = R * ( 1 + (r + E) * (1 - R / K) - a * C)
	dCdt = C * (1 - z + e * a * R)
	return sc.array([dRdt, dCdt])

def lv2(r, a, z, e):
	t = sc.linspace(0, 15, 1000)
	R0 = 10
	C0 = 5
	RC0 = sc.array([R0, C0])
	E = stats.norm.rvs(loc=0, scale=0.1, size=1000)
	pops, infodict = integrate.odeint(dCR_dt, RC0, E, full_output=True)
	f1 = p.figure()
	p.plot(pops[:,0], pops[:,1], 'r-')
	p.grid()
	p.xlabel('Resource density')
	p.ylabel('Consumer density')
	p.title('Consumer-Resource population dynamics')
	p.legend([f'r={r}\na={a}\nz={z}\ne={e}'])
	f1.savefig('../results/LV4.pdf')

r, a, z, e = float(sys.argv[1]), float(sys.argv[2]), float(sys.argv[3]), float(sys.argv[4])
lv2(r, a, z, e)