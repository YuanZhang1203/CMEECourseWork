#!usr/bin/env R
#################################################
# Title: 3(4) TPC Juvenlie Mortality Rate in R
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
data <- read.csv("../data/simple.csv")

zj<- subset(data,data$Variable=="Juvenile Mortality Rate (zJ)")
zj <- gdata::drop.levels(zj)

levels(zj$unit) # "days"; "proportion"(delete, as no time information)

# "Aedes notoscriptus" ### only one species
zj1<- subset(zj,zj$unit=="days")
zj1 <- gdata::drop.levels(zj1)
levels(zj1$species)
zj1 <- subset(zj,zj$species =="Aedes notoscriptus")
zj1 <- gdata::drop.levels(zj1)

zj1$stdvalue <- 1/zj1$traitvalue

zj1 <- ggplot(zj1, aes(x=temp, y = stdvalue)) 
zj1 <- zj1 + geom_point() + geom_smooth()+
  labs(title = "Juvenlie Mortality Rate: Aedes notoscriptus", 
       x = "Temperature (\u00B0C) ", 
       y = "Juvenlie Mortality Rate (1/day)")  
ggsave("../results/TPC_zj1.pdf", plot=zj1)


# other data in the adult mortality rate dataset (stage = Juvenile)
z <-  subset(data, data$Variable == "Adult Mortality Rate (z)")
z <- gdata::drop.levels(z)
z <- z %>% mutate(stagetype = case_when(stage %in% c("", "adult", "Adult", "adult (female)", "adult (male)", "Egg-to-adult") ~ 'adult',
                                        stage %in% c("Juvenile", "larvae", "Larvae", "larvae + pupae", "Egg to L1", "L1 to L2", "L2 to L3", "L3 to L4", "L4 to Pupae","Pupae to Adult" ) ~ 'Juvenile'))

zj_other <- subset(z,z$stagetype ==  "Juvenile") 
zj_other<- gdata::drop.levels(zj_other) 
levels(zj_other$unit) # "%"(delete, as no time information);  "event / (1 individual * 24 hour)"

j <- subset(zj_other,zj_other$unit ==  "event / (1 individual * 24 hour)") 
j <- gdata::drop.levels(j)

levels(j$species)

# 2 "Telenomus chrysopae" 
zj2 <- subset(j, j$species == "Telenomus chrysopae")
zj2 <- gdata::drop.levels(zj2)

zj2$stdvalue <- zj2$traitvalue

zj2 <- ggplot(zj2, aes(x=temp, y = stdvalue)) 
zj2 <- zj2 + geom_point() + geom_smooth()+
  labs(title = "Juvenlie Mortality Rate: Telenomus chrysopae", 
       x = "Temperature (\u00B0C) ", 
       y = "Juvenlie Mortality Rate (1/day)")  
ggsave("../results/TPC_zj2.pdf", plot=zj2)


# 3 "Telenomus lobatus"  
zj3 <- subset(j, j$species == "Telenomus lobatus")
zj3 <- gdata::drop.levels(zj3)

zj3$stdvalue <- zj3$traitvalue

zj3 <- ggplot(zj3, aes(x=temp, y = stdvalue)) 
zj3 <- zj3 + geom_point() + geom_smooth()+
  labs(title = "Juvenlie Mortality Rate: Telenomus lobatus", 
       x = "Temperature (\u00B0C) ", 
       y = "Juvenlie Mortality Rate (1/day)")  
ggsave("../results/TPC_zj3.pdf", plot=zj3)


#### combine
pdf_combine(c("../results/TPC_zj1.pdf", "../results/TPC_zj2.pdf", "../results/TPC_zj3.pdf"), 
            output = "../results/3.4.TPC_Juvenlie_Mortality_Rate.pdf")





