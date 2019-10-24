################################################################
################## Wrangling the Pound Hill Dataset ############
################################################################

############# Load the dataset ###############
# header = false because the raw data don't have real headers
MyData <- as.matrix(read.csv("../data/PoundHillData.csv",header = F)) 

# header = true because we do have metadata headers
MyMetaData <- read.csv("../data/PoundHillMetaData.csv",header = T, sep=";", stringsAsFactors = F)

############# Inspect the dataset ###############
head(MyData)
dim(MyData)
str(MyData)
#fix(MyData) #you can also do this
#fix(MyMetaData)

############# Transpose ###############
# To get those species into columns and treatments into rows 
MyData <- t(MyData) 
head(MyData)
dim(MyData)

############# Replace species absences with zeros ###############
MyData[MyData == ""] = 0

############# Convert raw matrix to data frame ###############

TempData <- as.data.frame(MyData[-1,],stringsAsFactors = F) #stringsAsFactors = F is important!
colnames(TempData) <- MyData[1,] # assign column names from original data

############# Convert from wide to long format  ###############
library(dplyr)
library(tidyr)

MyWrangledData = gather(TempData, key = "Species", value = "Count", names(TempData)[!(names(TempData) %in% c("Cultivation", "Block", "Plot", "Quadrat"))])
MyWrangledData = MyWrangledData %>% 
    mutate(Cultivation = as.factor(Cultivation),
           Block = as.factor(Block),
           Plot = as.factor(Plot),
           Quadrat = as.factor(Quadrat),
           Count = as.integer(Count))

str(MyWrangledData)
head(MyWrangledData)
dim(MyWrangledData)
