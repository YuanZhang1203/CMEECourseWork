#!usr/bin/env R
#################################################
# Title: 2.1 Data summary in R
# MSc CMEE 
# July 2020 
# Author: YUAN ZHANG 
# refer to: Alex_ Data Summary Code
#################################################
rm(list = ls())
graphics.off()

library("dplyr")
library("gridExtra")

# open datasset
data <- read.csv("../Data/TraitofInterest.csv")


data <- data %>% mutate(Variable = case_when(originaltraitname %in% c("Development Rate", "Development Time", "Development time","Generation Time","Egg development time") ~ 'Development Time (a)',
                                             originaltraitname %in% c("Fecundity","Fecundity Rate", "Oviposition Rate") ~ 'Peak Fecundity Rate (bpk)',
                                             originaltraitname %in% c("Adult longevity (female, bloodfed)", "Adult longevity (male)","Adult survival","Adult survival (female, bloodfed)", "Adult survival (male)", "Longevity","Mortality Rate","Percentage Survival","Survival Rate","Survivorship", "Survival Time") ~ 'Adult Mortality Rate (z)',
                                             originaltraitname %in% c("Juvenile survival","Juvenile survival ") ~ 'Juvenile Mortality Rate (zJ)',
                                             originaltraitname %in% c("Fecundity","Fecundity Rate", "Oviposition Rate") ~ 'Fecundity Loss Rate (k)'))  #Fecundity loss rate


# write a loop to summary data
species <- levels(data$interactor1)
summary <- data.frame(Order=vector(),Species=vector(),Originalname = vector(),Trait = vector(), Unit=vector(),Unitdef=vector(),Stage=vector(),Temperature_Difference=vector())

for(i in 1:length(species)){
  spsub <- subset(data, data$interactor1==species[i])
  spsub <-gdata::drop.levels(spsub)
  Factor <- levels(spsub$originaltraitname)
  
  for(j in 1:length(Factor)){
    sub <- subset(spsub,spsub$originaltraitname==Factor[j])
    sub<-gdata::drop.levels(sub)
    df<- data.frame(Order=sub$interactor1order[1], Species=sub$interactor1[1],Originalname = sub$originaltraitname[1], Trait = unique(sub$Variable), Unit=unique(sub$originaltraitunit), Unitdef=unique(sub$originaltraitdef),Stage=unique(sub$interactor1stage),Temperature_Difference=if(length(unique(sub$ambienttemp))>1)TRUE else FALSE)
    summary<-rbind(summary,df)
  }
  
}

write.csv(summary, file="../Data/summary.csv")

# write another loop to combine life stage 


