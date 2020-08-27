#!usr/bin/env R
###########################################################################################################
# Title: 4. example traits in R
# MSc CMEE 
# Aug 2020 
# Author: YUAN ZHANG 
############################################################################################################
# setwd("CMEECourseWork/Mainproject/code/")

rm(list = ls())
graphics.off()

#--------- Load some packages --------#
library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggforce)
library(pdftools)
library(ggpubr)
library(cowplot)
library(ggpmisc)
library(PerformanceAnalytics)

#--------- Take first species as example --------#
data <- read.csv("../data/simple.csv")
alldata <- read.csv("../data/TraitofInterest.csv")

# set equations 
f1 <- y ~ x + I(x^2)
f2 <- y ~ poly(x, 3, raw = TRUE)

# select species: 1  "Anthonomus grandis"
d1 <- subset(data, data$species == "Anthonomus grandis")
d1 <- gdata::drop.levels(d1)

# a 
a1 <- subset(d1, d1$Variable == "Development Time (a)")
a1 <- subset(a1, a1$originaltraitname == "Development Time")
unique(a1$stage) 
a1 <- subset(a1, a1$stage != "L1 - L3")
a1 <- subset(a1, a1$stage != "Immature Stages")

unique(a1$stage)

f_a1 <- ggplot(a1, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black', size = 1) +
  labs(title = "TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 0.8,se = T)  ## quadratic fit

plot1a <- f_a1 + facet_wrap_paginate(~stage, ncol=2, nrow=3, page=1, labeller = label_wrap_gen(30), scales = "free_y") +  
  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 13, face = "bold.italic")) 

ggsave("../results/4.example1.png", plot = plot1a)

### combine stages
a2 <- subset(d1, d1$Variable == "Development Time (a)")
a2 <- subset(a2, a2$stage == "Immature Stages")
a2 <- gdata::drop.levels(a2) 

f_a2 <- ggplot(a2, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/4.example2.png", plot = f_a2)

##### b
b1 <- subset(d1, d1$Variable == "Fecundity")
b1 <- gdata::drop.levels(b1)
b1$stdvalue <- b1$traitvalue
b1$logvalue <- log(b1$stdvalue)

# check units
unique(b1$unit) # "Eggs/female/day"

## plot b ~ time
bpk1 <- c()
k1 <- c()
temp1 <- unique(b1$temp)  # get temperature ranges: 15 20 25 30 35

for (i in 1: length(temp1)) {
  df <- b1[which(b1$temp == temp1[i]),]
  bpk1[i] = max(df$traitvalue)
  
  df$stdvalue <- df$traitvalue
  lm <- lm(logvalue ~ time, data = df)
  k1[i] <- coef(lm)[2]
}
bpk1 <- data.frame(bpk1, temp1, k1)

b1$stdtemp <- paste(b1$temp,"(\u00B0C)",sep = "" )
p_bt <- ggplot(b1, aes(x = time, y = stdvalue)) + 
  geom_point(show.legend = FALSE,color = 'black',size = 0.4) + theme_bw() + 
  labs(title = "Fecundity change with time", 
       x = "Time (days)", 
       y = "Fecundity (Eggs/female/day)")+
  theme(plot.title = element_text(hjust = 0.5,size = 13, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size= 10),
        axis.title.y=element_text(size= 10))

p_bt <- p_bt + facet_wrap(~ stdtemp, ncol = 2, scales = "free")+ geom_smooth(size = 0.8) 
ggsave("../results/4.example3.png", plot = p_bt)



## plot bpk ~ temperature
f_b1 <- ggplot(bpk1, aes(x = temp1, y = bpk1)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Peak Fecundity Rate (b)", 
       x = "Temperature (\u00B0C)", 
       y = "Fecundity (Eggs/female/day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 13, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/4.example4.png", plot = f_b1)



#### put together
p1 <- plot_grid(plot1a,f_a2,p_bt,f_b1,labels = c("A","B","C","D"), ncol=2,nrow=2)
# p1 <- annotate_figure(p1, top = text_grob("1. Anthonomus grandis", color = "red", face = "bold", size = 14))
ggsave("../results/4.example.png", plot = p1)


