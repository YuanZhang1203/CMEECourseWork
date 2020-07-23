#!usr/bin/env R
#################################################
# Title: 3(2) Peak_Fecundity_Rate and Fecundity Loss Rate in R
# MSc CMEE 
# July 2020 
# Author: YUAN ZHANG 
# refer to: TPC - Fecundity.ipynb
#################################################

rm(list = ls())
graphics.off()

library("dplyr")
library("ggplot2")
library("gridExtra")
library("pdftools")
data <- read.csv("../Data/TraitofInterest.csv")


data <- data %>% mutate(Variable = case_when(originaltraitname %in% c("Development Rate", "Development Time", "Development time","Generation Time","Egg development time") ~ 'Development Time (a)',
                                             originaltraitname %in% c("Fecundity","Fecundity Rate", "Oviposition Rate") ~ 'Peak Fecundity Rate (bpk)',
                                             originaltraitname %in% c("Adult longevity (female, bloodfed)", "Adult longevity (male)","Adult survival","Adult survival (female, bloodfed)", "Adult survival (male)", "Longevity","Mortality Rate","Percentage Survival","Survival Rate","Survivorship", "Survival Time") ~ 'Adult Mortality Rate (z)',
                                             originaltraitname %in% c("Juvenile survival","Juvenile survival ") ~ 'Juvenile Mortality Rate (zJ)',
                                             originaltraitname %in% c("Fecundity","Fecundity Rate", "Oviposition Rate") ~ 'Fecundity Loss Rate (k)'))  #Fecundity loss rate

fecund <- subset(data,data$Variable=="Peak Fecundity Rate (bpk)")
fecund <- gdata::drop.levels(fecund)

levels(fecund$originaltraitunit)
levels(fecund$interactor1)


##1. Aedes aegypti
aegypti <- subset(fecund, fecund$interactor1 =="Aedes aegypti")
aegypti <- gdata::drop.levels(aegypti)

levels(aegypti$originaltraitunit)

aegypti$stdvalue <- aegypti$originaltraitvalue
aegypti$stdunit <- "Eggs per female per day"
ncol(aegypti)

##2. Aedes albopictus
albop <- subset(fecund, fecund$interactor1 =="Aedes albopictus")
albop <- gdata::drop.levels(albop)

levels(albop$originaltraitunit)

write.csv(albop,file="../data/albop.csv")
#data inputted was incorrect, edited it manually
#From the original study, oviposition is measured by the number of eggs per gonotrophic cycle, I changed the original trait value into average number of eggs per female (paper lists eggs + number of individuals) and put the mean time per cycle as a secondary stressor

albop2 <-read.csv("../data/albop.csv")
albop2$stdvalue <- albop2$originaltraitvalue/albop2$secondstressorvalue
albop2$stdunit <- "Eggs per female per day"

##3. Corythucha ciliata
Cory <- subset(fecund, fecund$interactor1 =="Corythucha ciliata")
Cory <- gdata::drop.levels(Cory)

levels(Cory$originaltraitunit)

write.csv(Cory,file="../data/Cory.csv")
#Updated dataset manually to include length of ovoposition period as secondary stressor

Cory2 <- read.csv("../data/Cory.csv")
Cory2$stdvalue <- Cory2$originaltraitvalue/Cory2$secondstressorvalue
Cory2$stdunit <- "Eggs per female per day"


##4. Tetraneura nigri abdominalis
Tetra <- subset(fecund, fecund$interactor1 =="Tetraneura nigri abdominalis")
Tetra <- gdata::drop.levels(Tetra)

levels(Tetra$originaltraitunit)

Tetra$stdvalue <- Tetra$originaltraitvalue
Tetra$stdunit <- "Offspring per female per day"

#combining dataframes  
str(aegypti)
str(albop2)
str(Cory2)
str(Tetra)

aegypti <- aegypti[, ! names(aegypti) %in% "X"]
albop2 <- albop2[, ! names(albop2) %in% c("X","X.2","X.1")]
Cory2 <- Cory2[, ! names(Cory2) %in% c("X","X.1")]
Tetra <- Tetra[, ! names(Tetra) %in% "X"]

stdfecund <- rbind(aegypti,albop2,Cory2,Tetra)

fecundTPC <- ggplot(stdfecund, aes(x=ambienttemp, y=stdvalue)) + geom_point() + labs(title=expression(paste("Max Fecundity Rate ", (B[pk]))), x=expression(paste('Temperature (',~degree,'C)',sep='')), y=expression(paste("Fecundity Rate ", (Offpsring %*% Female^-1 %*% Day^-1)))) + facet_wrap(~ interactor1) + theme_bw() + theme(plot.title = element_text(hjust = 0.5))

fecundTPC
ggsave("../results/fecTPC1.pdf", plot=fecundTPC)


# With time series data
Time <- subset(fecund,fecund$interactor1 %in% c("Culex annulirostris","Anthonomus grandis","Aphis gossypii"))
Time <- gdata::drop.levels(Time)
Time$temp <- as.character(Time$ambienttemp)


Plot <- ggplot(Time, aes(x=secondstressorvalue, y=originaltraitvalue, colour=temp)) + geom_point() + theme_bw()+ labs(colour=expression(paste("Temperature( ", ~degree, "C)")), x= "Time (Days)", y=expression(paste("Fecundity Rate ", (Offpsring %*% Female^-1 %*% Day^-1))), title="Fecundity Rate") + theme(plot.title = element_text(hjust = 0.5))
TPC2<- Plot + facet_wrap(~ interactor1, nrow=2, scales="free")
ggsave("../results/fecTPC2.pdf", plot=TPC2)

pdf_combine(c("../results/fecTPC1.pdf", "../results/fecTPC2.pdf"), output="../results/3.2.TPC_Fecundity.pdf")



