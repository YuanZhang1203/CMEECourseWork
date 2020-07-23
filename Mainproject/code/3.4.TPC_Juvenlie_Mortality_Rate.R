#!usr/bin/env R
#################################################
# Title: 3(4) TPC Juvenlie Mortality Rate in R
# MSc CMEE 
# July 2020 
# Author: YUAN ZHANG 
#################################################

rm(list = ls())

library("dplyr")
library("ggplot2")
library("gridExtra")
library("ggforce")
library("pdftools")
data <- read.csv("../Data/TraitofInterest.csv")



data <- data %>% mutate(Variable = case_when(originaltraitname %in% c("Development Rate", "Development Time", "Development time","Generation Time","Egg development time") ~ 'Development Time (a)',
                                             originaltraitname %in% c("Fecundity","Fecundity Rate", "Oviposition Rate") ~ 'Peak Fecundity Rate (bpk)',
                                             originaltraitname %in% c("Adult longevity (female, bloodfed)", "Adult longevity (male)","Adult survival","Adult survival (female, bloodfed)", "Adult survival (male)", "Longevity","Mortality Rate","Percentage Survival","Survival Rate","Survivorship", "Survival Time") ~ 'Adult Mortality Rate (z)',
                                             originaltraitname %in% c("Juvenile survival","Juvenile survival ") ~ 'Juvenile Mortality Rate (zJ)',
                                             originaltraitname %in% c("Fecundity","Fecundity Rate", "Oviposition Rate") ~ 'Fecundity Loss Rate (k)'))  #Fecundity loss rate


dev <- subset(data,data$Variable=="Juvenile Mortality Rate (zJ)")
dev <- gdata::drop.levels(dev)

levels(dev$originaltraitunit)
levels(dev$interactor1)

# Aedes camptorhynchus
sp1 <- subset(dev,dev$interactor1=="Aedes camptorhynchus")
sp1 <- gdata::drop.levels(sp1)

levels(sp1$originaltraitunit)

sp1$stdvalue <- sp1$originaltraitvalue
sp1$stdunit <- "Days"
sp1$stdname <- sp1$interactor1

## Aedes notoscriptus

sp2 <- subset(dev,dev$interactor1=="Aedes notoscriptus")
sp2 <- gdata::drop.levels(sp2)

levels(sp2$originaltraitunit)
levels(sp2$interactor1stage)
levels(sp2$originaltraitdef)

sp2$stdvalue <- sp2$originaltraitvalue
sp2$stdunit <- "Days"
sp2$stdname <- paste(sp2$interactor1, "-", sp2$interactor1stage)

## Culex annulirostris
sp3 <- subset(dev,dev$interactor1=="Culex annulirostris")
sp3 <- gdata::drop.levels(sp3)

levels(sp3$originaltraitunit)

sp3$stdvalue <- sp3$originaltraitvalue
sp3$stdunit <- "Days"
sp3$stdname <- sp3$interactor1


## check 
ncol(sp1)
ncol(sp2)
ncol(sp3)

## combine the coclumn
zj <- rbind(sp1,sp2,sp3)

str(zj)

## plot
zjTPC <- ggplot(zj, aes(x=ambienttemp, y=stdvalue)) + geom_point() + theme_bw() + labs(title=expression(paste("Juvenlie Mortality Rate", (zJ))), x=expression(paste('Temperature (',~degree,'C)',sep='')), y="Juvenlie Mortality Rate") + theme(plot.title = element_text(hjust = 0.5)) + xlim(0,40) + ylim(0,120) 

plot1 <- zjTPC + facet_wrap_paginate(~stdname, ncol=2, nrow=2, page=1, labeller = label_wrap_gen(30)) +  theme(strip.text.x = element_text(size = 10))

ggsave("../results/3.4.TPC_Juvenlie_Mortality_Rate.pdf", plot=plot1)


