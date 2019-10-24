CMEECourseWork/Week3/
This is a folder about practices in Week3.


CMEECourseWork/Week3/Code/
This folder contain python codes of practices in Week3.
    --basic_io.R: A simple script to illustrate R input-output.  
    --break.R:Often it is useful to break out of a loop when some condition is met. 
    --control_flow.R: some examples of code exemplifying control flow tools in R. 
    --next.R: It could skip to next iteration of a loop. 
    --boilerplate.R: A boilerplate R script.  
    --TreeHeight.R: It belongs to practicals. It shows function calculates heights of trees given distance of each tree from its base and angle to its top, using  the trigonometric formula. It also create a csv output file called TreeHts.csv in results.
    --Vectorize1.R: It shows vectorization is an approach where you directly apply compiled, optimized code to run an operation on a vector, matrix, or an higher-dimensional data structure
    --preallocate.R: It shows "pre-allocate" a vector that fits all the values, R doesn't have to re-allocate memory each iteration, and the results can be much faster.
    --Apply1.R: It can be used when you want to apply a function to the rows or columns. 
    --Apply2.R: It can be used when you want to apply a function to a matrix. 
    --sample.R: An example of vectorization involving lapply and sapply. It also shows how sampling random numbers works.
    --Ricker.R: The Ricker model is a classic discrete population model which was introduced in 1954 by Ricker to model recruitment of stock in fisheries. It gives the expected number (or density) Nt+1 of individuals in generation t+1 as a function of the number of individuals in the previous generation t.
    --Vectorize2.R: It belongs to practicals. This is the stochastic Ricker model.
    --Vectorize1.py: It belongs to practicals(Extra Credit). The python versions of Vectorize1.R.
    --Vectorize2.py: It belongs to practicals(Extra Credit). The python versions of Vectorize2.R.
    --browse.R: An example using browser(), which is particularly handy because it will allow you to insert a breakpoint in your script.
    --TAutoCorr.R: It belongs to practicals.It loads and examines and plot KeyWestAnnualMeanTemperature.Rdata, using load().It also computes the appropriate correlation coefficient between successive years and store it.Then, It repeats this calculation 10000 times by -- randomly permuting the time series, and then recalculating the correlation coefficient for each randomly permuted year sequence and storing it.Moreover, it calculates what fraction of the correlation coefficients from the previous step were greater than that from step 1 (approximate p-value).
   --get_TreeHeight.R:It takes a csv file name from the command line.
   --get_TreeHeight.py:a python version of get_TreeHeight.R.
   --run_get_TreeHeight.sh:a test of get_TreeHeight.R and get_TreeHeight.py.
   --DataWrangTidy.R: It is a practical file. It uses dplyr and tidyr for the Wrangling the Pound Hill Dataset
   --PP_Lattice.R: It is a practical file. This script draws and saves three lattice graphs by feeding interaction type: one of predator mass, one of prey mass and one of the size ratio of prey mass over predator mass. 
   --Girko.R: It commands for plotting the Girko's law simulation.
   --plotLin.R: It shows mathematical annotation on a axis.
   --PP_Regress.R: It is a practical file. It draws and saves a pdf file of the PP_Regress_Figure.pdf, and writes the accompanying regression results to a formatted table in csv. 
   --GPDD_Data.R: It is a practical file. This script could:
Loads the maps package
Loads the GPDD data
Creates a world map (use the map function, read its help, also google examples using maps
Superimposes on the map all the locations from which we have data in the GPDD dataframe
   --PP_Regress_loc.R: It is a practical file(Extra Credit). The analysis this time should be separate by the dataset's Location field.




CMEECourseWork/Week3/Data/
This folder contain python relevant data of practices in Week3.
    --trees.csv: It shows data information of trees.
    --KeyWestAnnualMeanTemperature.RData:This is the temperature in Key West, Florida for the 20th century.
    --PoundHillData.csv: A data file from TheMulQuaBio's data directory
    --PoundHillMetaData.csv:A data file from TheMulQuaBio's data directory
    --EcolArchives-E089-51-D1.csv: As a case study, it is a dataset on Consumer-Resource (e.g., Predator-Prey) body mass ratios taken from the Ecological Archives of the ESA (Barnes et al. 2008, Ecology 89:881).
    --GPDDFiltered.RData:




CMEECourseWork/Week3/results/
This is a folder contains results files
    --MyData.csv: Output of the final line in the basic_io.R because files are covered.
    --TreeHts.csv: It is created by the TreeHeight.R. In result, it contains the calculated tree heights along with the original data in the following format (only first two rows and headers shown):
"Species","Distance.m","Angle.degrees","Tree.Height.m"
"Populus tremula",31.6658337740228,41.2826361937914,27.8021161438536
"Quercus robur",45.984992608428,44.5359166583512,45.2460250644405  
    --TAutoCorr.pdf: A output of the practical 3 in R. 
    --Pred_Lattice.pdf:The output of PP_Lattice.R.
    --Prey_Lattice.pdf: The output of PP_Lattice.R.
    --SizeRatio_Lattice.pdf: The output of PP_Lattice.R. 
    --PP_Results.csv: The output of PP_Lattice.R. It shows the mean and median log predator mass, prey mass, and predator-prey size ratio.
    --Girko.pdf: The output of Girko.R.
    --MyLinReg.pdf: The output of plotLin.R.
    --PP_Regress_Results.csv: It is calculated the regression results corresponding to the lines fitted in the figure and save it to a csv delimited table.
    --GPDD_map.pdf:The output of GPDD_Data.R.The map shows the locations are mainly in the north america and europe.
    --PP_Regress_Figure.pdf: The output of PP_Regress.R.
    --PP_Regress_Results.csv: The output of PP_Regress.R.
    --PP_Regress_loc_Results.csv: The output of PP_Regress_loc.R.


CMEECourseWork/Week3/Sandbox/
This is a folder contains test files
    --test1.R: some examples of functions with conditionals













   
   