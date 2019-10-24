import numpy as np
from datetime import datetime 

M = np.random.uniform(low=0, high=1, size=(1000, 1000))

def SumAllElements(x):
	Dimensions = x.shape
	Tot = 0
	for i in range(Dimensions[0]):
		for j in range(Dimensions[1]):
			Tot += x[i,j]
	return(Tot)

start_time = datetime.now()
SumAllElements(M)
time_elapsed = datetime.now() - start_time 
print('Elapsed time of SumAllElements function: {}'.format(time_elapsed))

start_time = datetime.now()
np.sum(M)
time_elapsed = datetime.now() - start_time 
print('Elapsed time of np.sum function: {}'.format(time_elapsed))