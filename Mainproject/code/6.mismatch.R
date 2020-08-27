#!usr/bin/env R
#################################################
# Title: 6. mismatch in R
# MSc CMEE 
# Auguest 2020 
# Author: YUAN ZHANG 
#################################################
rm(list = ls())
graphics.off()

data <- read.csv("../data/simple.csv")

# setwd("CMEECourseWork/Mainproject/code/")

# a # larger time, low growth rate. so I need to get a lowest development time, get the $minimum ~T
# z mortality rate, $minimum 
# zj $minimum 
# b $maximum
# k loss rate $minmum

# select species: 1,5, 9, 10, 11, 17 

#--------- Load some packages --------#
library(gdata) # read excel file
library(dplyr)
library(ggplot2)
library(ggpubr)

library(gridExtra)
library(ggforce)
library(pdftools)
library(cowplot)
library(ggpmisc)
library(PerformanceAnalytics)

#--------- set equations --------#
f1 <- y ~ x + I(x^2)

#--------- plot species separately --------#
# select species: 1  "Anthonomus grandis"
d1 <- subset(data, data$species == "Anthonomus grandis")
d1 <- gdata::drop.levels(d1)

a1 <- subset(d1, d1$Variable == "Development Time (a)")
a1 <- subset(a1, a1$originaltraitname == "Development Time")

f_a1 <- ggplot(a1, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black') +
  labs(title = "1. Anthonomus grandis : TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label..,..rr.label..,..AIC.label.., sep = "~~~")), # ..rr.label..,..AIC.label..
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2)

plot1a <- f_a1 + facet_wrap_paginate(~stage, ncol=3, nrow=3, page=1, labeller = label_wrap_gen(30), scales = "free_y") +  
          theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic")) 

ggsave("../results/mis_a1.pdf", plot = plot1a, width = 10, height = 10)

####  z
z1 <- subset(d1, d1$Variable == "Adult Mortality Rate (z)")
z1 <- subset(z1, z1$originaltraitname == "Longevity")
z1 <- gdata::drop.levels(z1)
z1$stdvalue <- 1/z1$traitvalue

f_z1 <- ggplot(z1, aes(x=temp, y = stdvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "1. Anthonomus grandis : TPC of Adult Mortality Rate (z)", 
       x = "Temperature (\u00B0C) ", 
       y = "Adult Mortality Rate (1/Day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2)+
  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic"))

ggsave("../results/mis_z1.pdf", plot = f_z1, width = 10, height = 10)

#### zj
zj1 <- subset(d1, d1$Variable == "Adult Mortality Rate (z)")
zj1 <- subset(zj1, zj1$stage != "")
zj1 <- gdata::drop.levels(zj1)

f_zj1 <- ggplot(zj1, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "1. Anthonomus grandis : TPC of Juvenile Mortality Rate (zJ)", 
       x = "Temperature (\u00B0C) ", 
       y = "Juvenile Mortality Rate (1/Day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2)

plotzj1 <- f_zj1 + facet_wrap_paginate(~stage, ncol=3, nrow=3, page=1, labeller = label_wrap_gen(30), scales = "free_y") +  
  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic"))
ggsave("../results/mis_zj1.pdf", plot = plotzj1, width = 10, height = 10)

#### b
b1 <- subset(d1, d1$Variable == "Fecundity")
b1 <- subset(b1, b1$time >= 12)
b1 <- subset(b1, b1$time <= 48)
b1 <- gdata::drop.levels(b1)
# b1$logvalue <- log(b1$stdvalue)

f_b1 <- ggplot(b1, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "1. Anthonomus grandis : TPC of Fecundity Rate (b)", 
       x = "Temperature (\u00B0C) ", 
       y = "Fecundity (Eggs/female/day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2)

plot_b1_1 <- f_b1 + facet_wrap_paginate(~time, ncol=3, nrow=3, page=1, labeller = label_wrap_gen(30), scales = "free_y") +  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic"))
plot_b1_2 <- f_b1 + facet_wrap_paginate(~time, ncol=3, nrow=3, page=2, labeller = label_wrap_gen(30), scales = "free_y") +  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic"))
plot_b1_3 <- f_b1 + facet_wrap_paginate(~time, ncol=3, nrow=3, page=3, labeller = label_wrap_gen(30), scales = "free_y") +  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic"))
plot_b1_4 <- f_b1 + facet_wrap_paginate(~time, ncol=3, nrow=3, page=4, labeller = label_wrap_gen(30), scales = "free_y") +  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic"))

ggsave("../results/mis_b1_1.pdf", plot = plot_b1_1, width = 10, height = 10)
ggsave("../results/mis_b1_2.pdf", plot = plot_b1_2, width = 10, height = 10)
ggsave("../results/mis_b1_3.pdf", plot = plot_b1_3, width = 10, height = 10)
ggsave("../results/mis_b1_4.pdf", plot = plot_b1_4, width = 10, height = 10)

pdf_combine(c("../results/mis_b1_1.pdf", "../results/mis_b1_2.pdf", "../results/mis_b1_3.pdf", "../results/mis_b1_4.pdf"), output = "../results/mis_b1.pdf")

#### k 
b1$logvalue <- log(b1$traitvalue)
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

# positive
bpk1$stdk1 <- -bpk1$k1

f_k1 <- ggplot(bpk1, aes(x=temp1, y = stdk1)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "1. Anthonomus grandis : TPC of Fecundity Loss Rate (k)", 
       x = "Temperature (\u00B0C) ", 
       y = "log of Fecundity Loss Rate")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2)+
  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic"))
      

ggsave("../results/mis_k1.pdf", plot = f_k1, width = 10, height = 10)

#### put together
pdf_combine(c("../results/mis_a1.pdf", "../results/mis_z1.pdf", "../results/mis_zj1.pdf","../results/mis_b1.pdf", "../results/mis_k1.pdf" ), output = "../results/mis_1.pdf")


# 5. Aedes albopictus
d5 <- subset(data, data$species == "Aedes albopictus")
d5 <- gdata::drop.levels(d5) # "Development Time (a)"     "Fecundity"       "Adult Mortality Rate (z)"

# 1) a 
a5 <- subset(d5, d5$Variable == "Development Time (a)")
a5$stdvalue <- 1/a5$traitvalue

f_a5 <- ggplot(a5, aes(x=temp, y = stdvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black') +
  labs(title = "5. Aedes albopictus : TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label..,..rr.label..,..AIC.label.., sep = "~~~")), # ..rr.label..,..AIC.label..
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2)
plot_a5 <- f_a5 + facet_wrap_paginate(~stage, ncol=1, nrow=1, page=1, labeller = label_wrap_gen(30), scales = "free_y") +  
  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic")) 

ggsave("../results/mis_a5.pdf", plot = plot_a5, width = 10, height = 10)

# 2) z
z5 <- subset(d5, d5$Variable == "Adult Mortality Rate (z)")
z5 <- subset(z5, z5$stage == "L1 to Adult")

f_z5 <- ggplot(z5, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "5. Aedes albopictus : TPC of Adult Mortality Rate (z)", 
       x = "Temperature (\u00B0C) ", 
       y = "Percentage Survival (%)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2)+
  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic"))

ggsave("../results/mis_z5.pdf", plot = f_z5, width = 10, height = 10)


# 3) zj
zj5 <- subset(d5, d5$Variable == "Adult Mortality Rate (z)")
zj5 <- subset(zj5, zj5$stage != "L1 to Adult")

f_zj5 <- ggplot(zj5, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "5. Aedes albopictus :TPC of Juvenile Mortality Rate (zJ)", 
       x = "Temperature (\u00B0C) ", 
       y = "Percentage Survival (%)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2)

plotzj5<- f_zj5 + facet_wrap_paginate(~stage, ncol=3, nrow=3, page=1, labeller = label_wrap_gen(30), scales = "free_y") +  
  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic"))
ggsave("../results/mis_zj5.pdf", plot = plotzj5, width = 10, height = 10)


# 4)  b
b5 <- subset(d5, d5$Variable == "Fecundity")
b5$stdvalue <- 1/b5$traitvalue
b5$no <- c("g1","g2","g3","g4","g1","g1", "g2","g2", "g3","g3", "g4","g4","g1","g2","g2","g3","g3","g4","g4","g1","g2")
  
f_b5 <- ggplot(b5, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "5. Aedes albopictus : TPC of Fecundity Rate (b)", 
       x = "Temperature (\u00B0C) ", 
       y = "Fecundity (Eggs/female/day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2)
plot_b5 <- f_b5 + facet_wrap_paginate(~no, ncol=2, nrow=2, page=1, labeller = label_wrap_gen(30), scales = "free_y") +  
  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic"))

ggsave("../results/mis_b5.pdf", plot = plot_b5, width = 10, height = 10)

#### put together
pdf_combine( c("../results/mis_a5.pdf", "../results/mis_z5.pdf", "../results/mis_zj5.pdf","../results/mis_b5.pdf"), output = "../results/mis_5.pdf")
  

# 9 Culex annulirostris 
d9 <- subset(data, data$species == "Culex annulirostris")
d9 <- gdata::drop.levels(d9)
unique(d9$Variable) # [1] "Juvenile Mortality Rate (zJ)" "Development Time (a)"         "Adult Mortality Rate (z)"    "Fecundity"   

# 1) a9
a9 <- subset(d9, d9$Variable == "Development Time (a)")
a9 <- subset(a9, a9$traitvalue <= 100)

f_a9 <- ggplot(a9, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black') +
  labs(title = "9. Culex annulirostris : TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label..,..rr.label..,..AIC.label.., sep = "~~~")), # ..rr.label..,..AIC.label..
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2)

plot_a9 <- f_a9 + facet_wrap_paginate(~stage, ncol=2, nrow=2, page=1, labeller = label_wrap_gen(30), scales = "free_y") +  
  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic")) 

ggsave("../results/mis_a9.pdf", plot = plot_a9, width = 10, height = 10)


# 2) z9
z9 <- subset(d9, d9$Variable == "Adult Mortality Rate (z)")
unique(z9$originaltraitname) # [1] "Adult longevity (female, bloodfed)" "Adult longevity (male)"            
# [3] "Adult survival (female, bloodfed)"  "Adult survival (male)"
z9_1 <- subset(z9, z9$originaltraitname == "Adult longevity (female, bloodfed)")
z9_1$stdvalue <- 1/z9_1$traitvalue
z9_2 <- subset(z9, z9$originaltraitname == "Adult longevity (male)")
z9_2$stdvalue <- 1/z9_2$traitvalue
z9_3 <- subset(z9, z9$originaltraitname == "Adult survival (female, bloodfed)")
z9_4 <- subset(z9, z9$originaltraitname == "Adult survival (male)")


f_z9_1 <- ggplot(z9_1, aes(x=temp, y = stdvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(x = "Temperature (\u00B0C) ", 
       y = "Adult Mortality Rate (1/Day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2)+
  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic"))

ggsave("../results/mis_z9_1.pdf", plot = f_z9_1, width = 10, height = 10)

f_z9_2 <- ggplot(z9_2, aes(x=temp, y = stdvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(
       x = "Temperature (\u00B0C) ", 
       y = "Adult Mortality Rate (1/Day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2)+
  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic"))

ggsave("../results/mis_z9_2.pdf", plot = f_z9_2, width = 10, height = 10)

f_z9_3 <- ggplot(z9_3, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(
       x = "Temperature (\u00B0C) ", 
       y = "Adult Mortality Rate (%)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2)+
  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic"))

ggsave("../results/mis_z9_3.pdf", plot = f_z9_3, width = 10, height = 10)

f_z9_4 <- ggplot(z9_4, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(x = "Temperature (\u00B0C) ", 
       y = "Adult Mortality Rate (%)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2)+
  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic"))

ggsave("../results/mis_z9_4.pdf", plot = f_z9_4, width = 10, height = 10)


#### put together_z
pf9 <- ggarrange(f_z9_1,f_z9_2,f_z9_3,f_z9_4)
pf9 <- annotate_figure(pf9, top = text_grob("9. Culex annulirostris: TPC of Adult Mortality Rate (z)", color = "black", face = "bold.italic", size = 14))
ggsave("../results/mis_z9.pdf", plot = pf9,  width = 10, height = 10)


# 3) zj9
zj9 <- subset(d9, d9$Variable == "Juvenile Mortality Rate (zJ)")

f_zj9 <- ggplot(zj9, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "9. Culex annulirostris: TPC of Juvenile Mortality Rate (zJ)", 
       x = "Temperature (\u00B0C) ", 
       y = "	Juvenile survival  (%)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2)

plotzj9 <- f_zj9 + facet_wrap_paginate(~ID, ncol=3, nrow=3, page=1, labeller = label_wrap_gen(30), scales = "free_y") +  
  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic"))
ggsave("../results/mis_zj9.pdf", plot = plotzj9, width = 10, height = 10)


# 4) b9
b9 <- subset(d9, d9$Variable == "Fecundity")

bpk9 <- c()
temp9 <- unique(b9$temp)  # get temperature ranges: 20 25 30
for (i in 1: length(temp9)) {
  df <- b9[which(b9$temp == temp9[i]),]
  bpk9[i] = max(df$traitvalue)
}
bpk9 <- data.frame(bpk9, temp9)

f_b9 <- ggplot(bpk9, aes(x=temp9, y = bpk9)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "9. Culex annulirostris: TPC of Fecundity Rate (b)", 
       x = "Temperature (\u00B0C) ", 
       y = "Fecundity (Eggs/female/day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2)

ggsave("../results/mis_b9.pdf", plot = f_b9, width = 10, height = 10)

#### put together
pdf_combine(c("../results/mis_a9.pdf", "../results/mis_z9.pdf", 
              "../results/mis_zj9.pdf","../results/mis_b9.pdf" ), output = "../results/mis_9.pdf")


# 10. Aphis gossypii
d10 <- subset(data, data$species == "Aphis gossypii")
d10 <- gdata::drop.levels(d10)
unique(d10$Variable) #"Development Time (a)"     "Adult Mortality Rate (z)" "Fecundity"

### 1) a       "Development Time (a)"   
a10 <- subset(d10, d10$Variable == "Development Time (a)")
a10 <- subset(a10, a10$stage != "adult")
a10 <- gdata::drop.levels(a10) 

a10_1 <- subset(a10, a10$originaltraitname == "Development Rate")
a10_1 <- gdata::drop.levels(a10_1) 
a10_1$stdvalue <- 1/a10_1$traitvalue
a10_1$stdunit <- "days"

a10_2 <- subset(a10, a10$originaltraitname != "Development Rate")
a10_2 <- gdata::drop.levels(a10_2) 
a10_2$stdvalue <- a10_2$traitvalue
a10_2$stdunit <- "days"

a10 <- rbind(a10_1, a10_2)

f_a10 <- ggplot(a10, aes(x=temp, y = stdvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black') +
  labs(title = "10. Aphis gossypii : TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label..,..rr.label..,..AIC.label.., sep = "~~~")), # ..rr.label..,..AIC.label..
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2)

plot_a10 <- f_a10 + facet_wrap_paginate(~ID, ncol=2, nrow=2, page=1, labeller = label_wrap_gen(30), scales = "free_y") +  
  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic")) 

ggsave("../results/mis_a10.pdf", plot = plot_a10, width = 10, height = 10)

### 2) z      "Adult Mortality Rate (z)" 
z10 <- subset(d10, d10$originaltraitname == "Longevity")
z10 <- gdata::drop.levels(z10)
z10$stdvalue <- 1/z10$traitvalue
z10$stdunit <- "1/day"

f_z10 <- ggplot(z10, aes(x=temp, y = stdvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "10. Aphis gossypii : TPC of Adult Mortality Rate (z)", 
         x = "Temperature (\u00B0C) ", 
         y = "Adult Mortality Rate (1/Day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2)+
  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic"))

ggsave("../results/mis_z10.pdf", plot = f_z10, width = 10, height = 10)

### 3) zJ 
zj10 <- subset(d10,d10$stage == "juvenile")
zj10 <- gdata::drop.levels(zj10)

f_zj10 <- ggplot(zj10, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "10. Aphis gossypii : TPC of Juvenile Mortality Rate (zJ)", 
       x = "Temperature (\u00B0C) ", 
       y = "	Juvenile Mortality Rate  (1/Day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2)

ggsave("../results/mis_zj10.pdf", plot = f_zj10, width = 10, height = 10)

## 4) Fecundity
b10 <- subset(d10, d10$Variable == "Fecundity")
b10 <- subset(b10, b10$time <= 26)

f_b10 <- ggplot(b10, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "10. Aphis gossypii : TPC of Fecundity Rate (b)", 
       x = "Temperature (\u00B0C) ", 
       y = "Fecundity (Eggs/female/day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2)

plot_b10_1 <- f_b10 + facet_wrap_paginate(~time, ncol=3, nrow=3, page=1, labeller = label_wrap_gen(30), scales = "free_y") +  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic"))
plot_b10_2 <- f_b10 + facet_wrap_paginate(~time, ncol=3, nrow=3, page=2, labeller = label_wrap_gen(30), scales = "free_y") +  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic"))
plot_b10_3 <- f_b10 + facet_wrap_paginate(~time, ncol=3, nrow=3, page=3, labeller = label_wrap_gen(30), scales = "free_y") +  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic"))

ggsave("../results/mis_b10_1.pdf", plot = plot_b10_1, width = 10, height = 10)
ggsave("../results/mis_b10_2.pdf", plot = plot_b10_2, width = 10, height = 10)
ggsave("../results/mis_b10_3.pdf", plot = plot_b10_3, width = 10, height = 10)

pdf_combine(c("../results/mis_b10_1.pdf", "../results/mis_b10_2.pdf", 
              "../results/mis_b10_3.pdf"), output = "../results/mis_b10.pdf")


#### put together
pdf_combine(c("../results/mis_a10.pdf", "../results/mis_z10.pdf", 
              "../results/mis_zj10.pdf","../results/mis_b10.pdf" ), 
              output = "../results/mis_10.pdf")


#### 11. Corythucha ciliata
d11 <- subset(data, data$species == "Corythucha ciliata")
d11 <- gdata::drop.levels(d11)
unique(d11$Variable) #"Development Time (a)"，"Fecundity"， "Adult Mortality Rate (z)"

### 1) a  "Development Time (a)"   
a11 <- subset(d11, d11$Variable == "Development Time (a)")
a11 <- subset(d11, d11$originaltraitname == "Development Time")
a11 <- gdata::drop.levels(a11) 

f_a11 <- ggplot(a11, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black') +
  labs(title = "11. Corythucha ciliata : TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label..,..rr.label..,..AIC.label.., sep = "~~~")), # ..rr.label..,..AIC.label..
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2)

plot_a11 <- f_a11 + facet_wrap_paginate(~stage, ncol=3, nrow=3, page=1, labeller = label_wrap_gen(30), scales = "free_y") +  
  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic")) 

ggsave("../results/mis_a11.pdf", plot = plot_a11, width = 10, height = 10)

# 2) "Adult Mortality Rate (z)"
z11 <- subset(d11, d11$Variable == "Adult Mortality Rate (z)")
z11 <- subset(z11, z11$originaltraitname == "Longevity")
z11 <- gdata::drop.levels(z11)
z11$stdvalue <- 1/z11$traitvalue

f_z11 <- ggplot(z11, aes(x=temp, y = stdvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "11. Corythucha ciliata : TPC of Adult Mortality Rate (z)", 
       x = "Temperature (\u00B0C) ", 
       y = "Adult Mortality Rate (1/Day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2)+
  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic"))

ggsave("../results/mis_z11.pdf", plot = f_z11, width = 10, height = 10)



# 3) zj
zj11 <- subset(d11, d11$Variable == "Adult Mortality Rate (z)")
zj11 <- subset(zj11, zj11$originaltraitname == "Percentage Survival")
zj11 <- subset(zj11, zj11$stage != "Egg-to-adult")

f_zj11 <- ggplot(zj11, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "11. Corythucha ciliata : TPC of Juvenile Mortality Rate (z)", 
       x = "Temperature (\u00B0C) ", 
       y = "Juvenile Mortality Survival (%)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2)+
  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic"))

plot_zj11 <- f_zj11 + facet_wrap_paginate(~stage, ncol=3, nrow=3, page=1, labeller = label_wrap_gen(30), scales = "free_y") +  
  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic")) 

ggsave("../results/mis_zj11.pdf", plot = plot_zj11, width = 10, height = 10)


# 4)b 
b11 <- subset(d11, d11$Variable == "Fecundity")
b11 <- gdata::drop.levels(b11)

f_b11 <- ggplot(b11, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "11. Corythucha ciliata :TPC of Fecundity Rate (b)", 
       x = "Temperature (\u00B0C) ", 
       y = "Fecundity (Eggs/female/day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2)+
  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic"))

ggsave("../results/mis_b11.pdf", plot = f_b11, width = 10, height = 10)


#### 
pdf_combine(c("../results/mis_a11.pdf", "../results/mis_z11.pdf", 
              "../results/mis_zj11.pdf","../results/mis_b11.pdf" ), output = "../results/mis_11.pdf")


#### 17. Telenomus isis
d17 <- subset(data, data$species == "Telenomus isis")
d17 <- gdata::drop.levels(d17)
unique(d17$Variable) #"Development Time (a)"     "Adult Mortality Rate (z)"

# 1) a       "Development Time (a)"   
a17 <- subset(d17, d17$Variable == "Development Time (a)")
a17 <- gdata::drop.levels(a17) 
a17$stdvalue <- 1/a17$traitvalue

f_a17 <- ggplot(a17, aes(x=temp, y = stdvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black') +
  labs(title = "17. Telenomus isis : TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label..,..rr.label..,..AIC.label.., sep = "~~~")), # ..rr.label..,..AIC.label..
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2)

plot_a17 <- f_a17 + facet_wrap_paginate(~ID, ncol=3, nrow=3, page=1, labeller = label_wrap_gen(30), scales = "free_y") +  
  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic")) 

ggsave("../results/mis_a17.pdf", plot = plot_a17, width = 10, height = 10)


# 2) z
z17 <- subset(d17, d17$Variable == "Adult Mortality Rate (z)")
z17 <- gdata::drop.levels(z17)
unique(z17$stage) # "juvenile"       "adult (female)"
z17 <- subset(z17,z17$stage == "adult (female)")
z17$stdvalue <- 1/ z17$traitvalue

f_z17 <- ggplot(z17, aes(x=temp, y = stdvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "17. Telenomus isis :  TPC of Adult Mortality Rate (z)", 
       x = "Temperature (\u00B0C) ", 
       y = "Adult Mortality Rate (1/Day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2)+
  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic"))

plot_z17 <- f_z17 + facet_wrap_paginate(~ID, ncol=3, nrow=3, page=1, labeller = label_wrap_gen(30), scales = "free_y") +  
  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic"))

ggsave("../results/mis_z17.pdf", plot = plot_z17, width = 10, height = 10)


# 3) zj
zj17 <- subset(d17, d17$Variable == "Adult Mortality Rate (z)")
zj17 <- gdata::drop.levels(zj17)
zj17 <- subset(zj17,zj17$stage == "juvenile")


f_zj17 <- ggplot(zj17, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "17. Telenomus isis :  TPC of Juvenile Mortality Rate (zJ)", 
       x = "Temperature (\u00B0C) ", 
       y = "Juvenile Mortality Rate (1/Day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2)+
  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic"))

plot_zj17 <- f_zj17 + facet_wrap_paginate(~ID, ncol=3, nrow=3, page=1, labeller = label_wrap_gen(30), scales = "free_y") +  
  theme(strip.text.x = element_text(size = 10), plot.title = element_text(hjust = 0.5,size = 14, face = "bold.italic"))

ggsave("../results/mis_zj17.pdf", plot = plot_zj17, width = 10, height = 10)

#### put together
pdf_combine( c("../results/mis_a17.pdf", "../results/mis_zj17.pdf", 
               "../results/mis_z17.pdf"), output = "../results/mis_17.pdf")


#--------- plot mismatch --------#
mis <- read.xls("../data/mis.xlsx")

library(ggpubr)

# Create bar plots of means
p_mis1 <- ggbarplot(mis, x = "Stage", y = "Topt", 
              add = c("mean_se", "jitter"),
              color = "Stage", palette = c("#00AFBB", "#E7B800"),
              position = position_dodge(0.8))+stat_compare_means(method = "t.test") # Display the significance level instead of the p-value
p_mis1 <-   p_mis1 + labs(x = "Stage", y = "Thermal optimum in Temperature (\u00B0C)")
plot_mis1 <- p_mis1 + facet_wrap_paginate(~ Species, ncol=3, nrow=2, page=1, labeller = label_wrap_gen(30))+ 
             theme(strip.text.x = element_text(size = 12))

ggsave("../results/2.mis1.png", plot = plot_mis1, width = 10, height = 8)



p_mis2 <- ggplot(mis, aes(Stage, Topt)) +
            geom_boxplot(aes(color = Stage)) +
            scale_color_manual(values = c("#00AFBB", "#E7B800"))+
            stat_compare_means(method = "t.test")+ # Display the significance level instead of the p-value
            labs(x = "Stage", y = "Thermal optimum in Temperature (\u00B0C)")
plot_mis2 <- p_mis2 + facet_wrap_paginate(~ Species, ncol=3, nrow=2, page=1, labeller = label_wrap_gen(30))+ 
              theme(strip.text.x = element_text(size = 12))
ggsave("../results/2.mis2.png", plot = plot_mis2, width = 10, height = 8)





