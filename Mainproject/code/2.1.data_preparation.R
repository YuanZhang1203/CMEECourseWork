#!usr/bin/env R
#################################################
# Title: 1. Data preparation in R
# MSc CMEE 
# Jun 2020 
# Author: YUAN ZHANG 
# refer to: Alex_Combining datasets.ipynb 
#################################################

rm(list = ls())
graphics.off()

# getwd()
# setwd("CMEECourseWork/Mainproject/code/")

########## 0. Preparation ##########
# install.packages("gdata") 

# library useful packges 
library("gdata")
library("dplyr")
library("gridExtra")
library("ggforce")
library("pdftools")

# load file; there are two files from Alex and (?)ceramai
Vec <- read.csv("../data/VecTraits.csv", header=T)
cera<- read.csv("../data/ceramai_digitised_dataset.csv", header=T)

########## 1. Processing ##########
#### 1) filter the insects in the "Vec"; no need to filter the cera 
subdata <- subset(Vec,Vec$published_data.interactor1class=="Insecta") 
# remove redundant levels. Sometimes there are fewer data after processing, but factor levels remain the same. 
subdata<-drop.levels(subdata) ## "gdata::drop.levels" could be used immidately without library 

#### 2) check and see list of species
levels(subdata$published_data.interactor1order) 
levels(cera$interactor1order)

# delete NA in order of cera (there is one order do not have name)
cera <- subset(cera, cera$interactor1order != "")
cera <-drop.levels(cera) 

#### 3) Combine Data
#standardizing column names #######
names(subdata) <- gsub("published_data.","",names(subdata)) # delete "published_data.", e.g. let first column names "originalid", same as "cer" 
names(subdata) <- gsub("locationtext","location",names(subdata))
cera<- cera[, ! names(cera) %in% c("published","embargorelease"), drop = F] #unknown (?)

#have a look (? ) why there is no temperature and time in cera??
#str(subdata)
#str(cera)

#changing timestart in Cera's dataset to the secondstressor column to match Vectraits
cera$secondstressorvalue <- cera$timestart
cera$secondstressorunit <- cera$totalobstimeunit

#changing some of Cera's data so that it has standardized secondary stressor of Time when there is time data
cera <- cera %>% mutate(secondstressor = case_when(secondstressorunit %in% c("day","days") ~ 'Time'))

data<- rbind(subdata,cera) # combine them
write.csv(data, file="../data/Fulldataset.csv")
levels(data$originaltraitname)

subset <- subset(data,data$originaltraitname %in% c("Adult longevity (female, bloodfed)", "Adult longevity (male)","Adult survival","Adult survival (female, bloodfed)","Adult survival (male)", "Development Rate", "Development Time", "Development time", "Fecundity","Fecundity Rate","Generation Time", "Juvenile survival","Juvenile survival ","Longevity","Mortality Rate","Oviposition Rate", "Percentage Survival", "Survival Rate","Survivorship", "Survival Time","Egg development time", "habitat"))
subset<- gdata::drop.levels(subset)

levels(subset$originaltraitname)
levels(subset$interactor1)



# output the file : Trait of Interest

write.csv(subset,file="../data/TraitofInterest.csv")


simple <- data.frame(ID= subset$originalid, order = subset$interactor1order, species = subset$interactor1, stage = subset$interactor1stage, originaltraitname = subset$originaltraitname, def = subset$originaltraitdef, traitvalue = subset$originaltraitvalue, unit = subset$originaltraitunit,temp = subset$ambienttemp, time = subset$timestart, location = subset$location, longitude = subset$longitude, latitude = subset$latitude, study = subset$citation, stringsAsFactors = FALSE)

simple <- simple %>% mutate(Variable = case_when(originaltraitname %in% c("Development Rate", "Development Time", "Development time","Generation Time") ~ 'Development Time (a)',
                                             originaltraitname %in% c("Fecundity","Fecundity Rate", "Oviposition Rate") ~ 'Fecundity',
                                             originaltraitname %in% c("Adult longevity (female, bloodfed)", "Adult longevity (male)","Adult survival","Adult survival (female, bloodfed)", "Adult survival (male)", "Longevity","Mortality Rate","Percentage Survival","Survival Rate","Survivorship", "Survival Time") ~ 'Adult Mortality Rate (z)',
                                             originaltraitname %in% c("Juvenile survival","Juvenile survival ") ~ 'Juvenile Mortality Rate (zJ)'))


write.csv(simple,file="../data/simple.csv")




