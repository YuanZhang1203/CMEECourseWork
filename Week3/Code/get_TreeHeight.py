import sys
import re
import numpy as np
import pandas as pd

InputFileName = sys.argv[1]
OutputFileName = re.sub("^.*/", "", InputFileName)
OutputFileName = re.sub("\.csv", "", OutputFileName)

dat = pd.read_csv(InputFileName)
dat.loc[:, 'Tree.Height.m'] = 0

def TreeHeight(degrees, distance):
	radians = degrees * np.pi / 180
	height = distance * np.tan(radians)
	return (height)

for i in range(dat.shape[0]):
	dat.loc[i, 'Tree.Height.m'] = TreeHeight(dat.loc[i,'Angle.degrees'], dat.loc[i,'Distance.m'])

dat.to_csv("../results/"+OutputFileName+"_treeheights_py.csv")
