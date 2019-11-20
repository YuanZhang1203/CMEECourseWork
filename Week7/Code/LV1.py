import matplotlib.pylab as p
import scipy.integrate as integrate
import scipy as sc

#define a function that returns the growth rate of consumer and resource population at any given time step.
def dCR_dt(pops, t=0):
	R = pops[0]
	C = pops[1]
	#assign some parameter values	
	r = 1.
	a = 0.1
	z = 1.5
	e = 0.75
	dRdt = r * R - a * R * C
	dCdt = -z * C + e * a * R * C
	return sc.array([dRdt, dCdt])	

#Define the time vector; let's integrate from time point 0 to 15, using 1000 sub-divisions of time:
t = sc.linspace(0, 15, 1000)
#Set the initial conditions for the two populations (10 resources and 5 consumers per unit area), and convert the two into an array (because our dCR_dt function take an array as input).
R0 = 10
C0 = 5
RC0 = sc.array([R0, C0])
#numerically integrate this system forward from those starting conditions:
pops, infodict = integrate.odeint(dCR_dt, RC0, t, full_output=True)

#open an empty figure object 
f1 = p.figure(1)
p.plot(t, pops[:,0], 'g-', label='Resource density') # Plot
p.plot(t, pops[:,1]  , 'b-', label='Consumer density')
p.grid()
p.legend(loc='best')
p.xlabel('Time')
p.ylabel('Population density')
p.title('Consumer-Resource population dynamics')
#p.show()# To display the figure
f1.savefig('../results/LV_model.pdf') #Save figure

f2 = p.figure(2)
p.plot(pops[:,0], pops[:,1], 'r-', color='red')
p.grid()
p.xlabel('Resource density')
p.ylabel('Consumer density')
p.title('Consumer-Resource population dynamics')
f2.savefig('../results/LV_model2.pdf')
	
