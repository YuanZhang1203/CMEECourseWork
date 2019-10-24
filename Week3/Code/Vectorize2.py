import numpy as np
from datetime import datetime 

def stochrick(p0=np.random.uniform(0.5,1.5,1000), r=1.2, K=1, sigma=0.2, numyears=100):
	N = np.empty([numyears, len(p0)])
	N[0,] = p0
	
	for j in range(len(p0)):
		for i in range(1,numyears):
			N[i,j] = N[i-1,j] * np.exp(r*(1-N[i-1,j]/K)+np.random.normal(0,sigma,1))
	
	return(N)

start_time = datetime.now()
stochrick()
time_elapsed = datetime.now() - start_time 
print('Stochastic Ricker takes:: {}'.format(time_elapsed))

def stochrickvect(p0=np.random.uniform(0.5,1.5,1000), r=1.2, K=1, sigma=0.2, numyears=100):
	N = np.empty([numyears, len(p0)])
	N[0,] = p0
	
	for i in range(1,numyears):
		N[i,] = N[i-1,] * np.exp(r*(1-N[i-1,]/K)+np.random.normal(0,sigma,len(p0)))
	
	return(N)

start_time = datetime.now()
stochrickvect()
time_elapsed = datetime.now() - start_time 
print('Vectorized Stochastic Ricker takes:: {}'.format(time_elapsed))