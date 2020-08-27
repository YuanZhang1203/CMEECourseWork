#!usr/bin/env R
#################################################
# Title: 3. em_T plot in R for 6 species
# MSc CMEE 
# Auguest 2020 
# Author: YUAN ZHANG 
#################################################
rm(list = ls())
graphics.off()

#--------- Load some packages --------#
library(ggplot2)

#--------- Load some files and store data --------#
source("3.rmT_1. Anthonomus grandis.R")

df1 <- data.frame(T = T_vec-273.15,rm = r_m_vector)
df1$Species <- "1. Anthonomus grandis"
  
source("3.rmT_5. Aedes albopictus.R")
df2 <- data.frame(T = T_vec-273.15,rm = r_m_vector)
df2$Species <- "5. Aedes albopictus"

source("3.rmT_9. Culex annulirostris.R")
df3 <- data.frame(T = T_vec-273.15,rm = r_m_vector)
df3$Species <- "9. Culex annulirostris"

source("3.rmT_10. Aphis gossypii.R")
df4 <- data.frame(T = T_vec-273.15,rm = r_m_vector)
df4$Species <- "10. Aphis gossypii"

source("3.rmT_11.Corythucha ciliata.R")
df5 <- data.frame(T = T_vec-273.15,rm = r_m_vector)
df5$Species <- "11.Corythucha ciliata"

source("3.rmT_17. Telenomus isis.R")
df6 <- data.frame(T = T_vec-273.15,rm = r_m_vector)
df6$Species <- "17. Telenomus isis"

df <- rbind(df1,df2,df3,df4,df5,df6)

#--------- plot rm --------#

p_rm <- ggplot(df, aes(x=T, y = rm)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'grey',size = 1) +
  geom_smooth()+
  labs(x = "Temperature (\u00B0C) ", y = expression(r[m]))  

plot_rm <- p_rm + facet_wrap_paginate(~Species, ncol=3, nrow=2, page=1, labeller = label_wrap_gen(30), scales = "free_y") +  
  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic")) 

ggsave("../results/3.rm_T.png", plot = plot_rm, width = 10, height = 6)
























