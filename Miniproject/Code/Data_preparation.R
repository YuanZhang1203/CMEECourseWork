#!usr/bin/env R
#################################################
# Title: Data preparation in R
# CMEE MSc March 2020 
# Author: YUAN ZHANG 
#################################################

rm(list = ls())
graphics.off()

#load packages
library("ggplot2")
library("minpack.lm")
library("zoo")
library("dplyr")
library("nls.multstart")


############ Create unique IDs to identify unique growth curves ############
# Input original dataset and modified the set 
o_data <- read.csv("../Data/LogisticGrowthData.csv")

# Get the information of units of the response variable 
# print(unique(o_data$PopBio_units))

# Get the information of units of the independence variable 
# print(unique(o_data$Time))

# infer single growth curves by combining Species, Medium, Temp and Citation columns (each species-medium-citation combination is unique)
o_data$ID <-  paste(o_data$Species, as.character(o_data$Temp), o_data$Medium, sep="_") # as.character is used to convert to string
modified_data <-data.frame(o_data$ID, o_data$Time, o_data$PopBio, o_data$Time_units, o_data$PopBio_units, o_data$Species, o_data$Temp)
names(modified_data) = c("ID","t", "N", "Time_units", "PopBio_units", "Species", "Temperature")


############ Filter the suitable data in a subdata ############

# remove NA and negetive value
modified_data <- modified_data[which((modified_data$t > 0) & (modified_data$N > 0)), ]

# remove the duplicated data
modified_data <- distinct(modified_data)

# filter out and remove datasets with less than 5 data points where 5 is the minimum number of data points needed to fit the models
ID_number <- data.frame(table(modified_data$ID))
names(ID_number) = c("ID", "Number")
for (i in 1:length(ID_number$Number)){
    ID_number <- ID_number[which(ID_number$Number < 5), ]
}
c = ID_number$ID
modified_data <- modified_data[ ! modified_data$ID %in% c, ] # remove 

write.csv(modified_data, "../Data/modified_data.csv") #svae as modified data in the Data folder









