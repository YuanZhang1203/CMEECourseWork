#!usr/bin/env R
#################################################
# Title: 3(3) TPC_adult mortality rate in R
# MSc CMEE 
# July 2020 
# Author: YUAN ZHANG 
#################################################

rm(list = ls())
graphics.off()

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

z <- subset(data,data$Variable=="Adult Mortality Rate (z)")
z <- gdata::drop.levels(z)

levels(z$originaltraitunit)
levels(z$interactor1order)
levels(z$interactor1)

## 1 "Coleoptera" 
Coleoptera <- subset(z,z$interactor1order == "Coleoptera")
Coleoptera <- gdata::drop.levels(Coleoptera)
levels(Coleoptera$interactor1)

# 1) "Anthonomus grandis"  
grandis  <- subset(Coleoptera,Coleoptera$interactor1 == "Anthonomus grandis")
grandis  <- gdata::drop.levels(grandis)
levels(grandis$originaltraitunit)

z1a <-  subset(grandis,grandis$originaltraitunit == "%"|grandis$originaltraitunit == "% of live weevils per day" )
z1a  <- gdata::drop.levels(z1a)

z1a$stdunit <- "%"
z1a$stdvalue <- z1a$originaltraitvalue
z1a$stdname  <- paste(unique(z1a$ambienttemp),"celcius")

p15 <- ggplot(A15, aes(x = time, y = stdvalue))
A1 <- p15 + geom_point() + geom_smooth()+
  labs(title=expression(paste('Anthonomus grandis 15 (',~degree,'C)',sep='')), 
       x = "Time (days)", 
       y = "Fecundity (Eggs/female/day)")  
  
# 2) "Stethorus punctillum"


## 2 "Diptera"    
## 3 "Hemiptera"  
## 4 "Hymenoptera" 
## 5 "Prostigmata" 
## 6 "Psocoptera" 

## Culex annulirostris
z1 <- subset(dev,dev$interactor1=="Culex annulirostris")
z1 <- gdata::drop.levels(z1)

levels(z1$originaltraitunit)
levels(z1$interactor1stage)
levels(z1$originaltraitdef)

# "Estimated daily survival rate (proportion parous raised to power of 1/gonotrophic cycle)"  
z1a <- subset(z1,z1$originaltraitdef=="Estimated daily survival rate (proportion parous raised to power of 1/gonotrophic cycle)")
z1a <- gdata::drop.levels(z1a)
levels(z1a$originaltraitunit)
z1a$stdvalue <- 1/z1a$originaltraitvalue
z1a$stdunit <- "Days"
z1a$stdname <- paste(z1a$interactor1, "-", "Estimated daily survival rate (proportion parous raised to power of 1/gonotrophic cycle)")

# "Length of adult lifespan (female, bloodfed)"     
z1b <- subset(z1,z1$originaltraitdef=="Length of adult lifespan (female, bloodfed)")
z1b <- gdata::drop.levels(z1b)
levels(z1b$originaltraitunit)
z1b$stdvalue <- z1b$originaltraitvalue
z1b$stdunit <- "Days"
z1b$stdname <- paste(z1b$interactor1, "-","Length of adult lifespan (female, bloodfed)" )

# "Length of adult lifespan (male)"   
z1c <- subset(z1,z1$originaltraitdef=="Length of adult lifespan (male)")
z1c <- gdata::drop.levels(z1c)
levels(z1c$originaltraitunit)
z1c$stdvalue <- z1c$originaltraitvalue
z1c$stdunit <- "Days"
z1c$stdname <- paste(z1c$interactor1, "-","Length of adult lifespan (male)")

# "Proportion surviving (max = prop. surviving from pupation to adult emergence: 0.57 for 20C)"
z1d <- subset(z1,z1$originaltraitdef=="Proportion surviving (max = prop. surviving from pupation to adult emergence: 0.57 for 20C)")
z1d <- gdata::drop.levels(z1d)
levels(z1d$originaltraitunit)
z1d$stdvalue <- z1d$originaltraitvalue
z1d$stdunit <- "Days"
z1d$stdname <- paste(z1d$interactor1, "-","Proportion surviving (max = prop. surviving from pupation to adult emergence: 0.57 for 20C)")

# "Proportion surviving (max = prop. surviving from pupation to emergence: 0.56 for 30C)"      
z1e <- subset(z1,z1$originaltraitdef=="Proportion surviving (max = prop. surviving from pupation to emergence: 0.56 for 30C)")
z1e <- gdata::drop.levels(z1e)
levels(z1e$originaltraitunit)
z1e$stdvalue <- z1e$originaltraitvalue
z1e$stdunit <- "Days"
z1e$stdname <- paste(z1e$interactor1, "-","Proportion surviving (max = prop. surviving from pupation to emergence: 0.56 for 30C)")

# "Proportion surviving (max = prop. surviving from pupation to emergence: 0.65 for 25C)" 
z1f <- subset(z1,z1$originaltraitdef=="Proportion surviving (max = prop. surviving from pupation to emergence: 0.65 for 25C)" )
z1f <- gdata::drop.levels(z1f)
levels(z1f$originaltraitunit)
z1f$stdvalue <- z1f$originaltraitvalue
z1f$stdunit <- "Days"
z1f$stdname <- paste(z1f$interactor1, "-","Proportion surviving (max = prop. surviving from pupation to emergence: 0.65 for 25C)" )


## combine
z1 <- rbind(z1a,z1b,z1c,z1d,z1e,z1f)

## plot
TPC_z <- ggplot(z1, aes(x=ambienttemp, y=stdvalue)) + geom_point() + theme_bw() + labs(title=expression(paste("3.3.TPC_adult mortality rate ", (z))), x=expression(paste('Temperature (',~degree,'C)',sep='')), y="adult mortality rate") + theme(plot.title = element_text(hjust = 0.5)) + xlim(0,40) + ylim(0,1) 
plot1 <- TPC_z + facet_wrap_paginate(~stdname, ncol=3, nrow=3, page=1, labeller = label_wrap_gen(30)) +  theme(strip.text.x = element_text(size = 10))

ggsave("../results/TPC1_z.pdf", plot=plot1)


ggsave("../results/3.3.TPC_adult mortality rate.pdf", plot=plot1)




