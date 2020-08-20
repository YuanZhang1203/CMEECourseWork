#!usr/bin/env R
###########################################################################################################
# Title: 4. linear model fitting in R
# MSc CMEE 
# Aug 2020 
# Author: YUAN ZHANG 
############################################################################################################

# setwd("CMEECourseWork/Mainproject/code/")

rm(list = ls())
graphics.off()

#--------- Load some packages --------#
library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggforce)
library(pdftools)
library(ggpubr)
library(cowplot)
library(ggpmisc)
library(PerformanceAnalytics)


f1 <- y ~ x + I(x^2)
f2 <- y ~ poly(x, 3, raw = TRUE)

data <- read.csv("../data/simple.csv")
unique((data$species))
#------------------------------------------------"Coleoptera"-----------------------------------------------------------------#
#### 1.  "Anthonomus grandis" ####

d1 <- subset(data, data$species == "Anthonomus grandis")
d1 <- gdata::drop.levels(d1)
unique(d1$Variable) #

# lm1 = lm(traitvalue~temp + I(temp^2),data = a1)
# su1 = summary(lm1)

#### 1)  a
a1 <- subset(d1, d1$Variable == "Development Time (a)")
a1 <- subset(a1, a1$stage == "Immature Stages")
a1 <- gdata::drop.levels(a1) 


f_a1 <- ggplot(a1, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_a1.pdf", plot = f_a1)


#### 2)  z1
z1 <- subset(d1, d1$Variable == "Adult Mortality Rate (z)")
z1 <- subset(z1, z1$originaltraitname == "Longevity")
z1 <- gdata::drop.levels(z1)
z1$stdvalue <- 1/z1$traitvalue

f_z1 <- ggplot(z1, aes(x=temp, y = stdvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Adult Mortality Rate (z)", 
       x = "Temperature (\u00B0C) ", 
       y = "Adult Mortality Rate (1/day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_z1.pdf", plot = f_z1)


#### 3) zj1
# no information

#### 4) b1
#######################
#### Fecundity b & k
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
###########################

f_b1 <- ggplot(bpk1, aes(x = temp1, y = bpk1)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Peak Fecundity Rate (b)", 
       x = "Temperature (\u00B0C)", 
       y = "Fecundity (Eggs/female/day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_b1.pdf", plot = f_b1)

#### 5) k1
f_k1 <- ggplot(bpk1, aes(x = temp1, y = k1)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Fecundity Loss Rate (k)", 
       x = "Time (days)", 
       y = "Fecundity Loss Rate")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_k1.pdf", plot = f_k1)

#### put together
pf1 <- ggarrange(f_a1,f_z1,f_b1,f_k1,labels = c("a","z","b","k"))
pf1 <- annotate_figure(pf1, top = text_grob("1. Anthonomus grandis", color = "red", face = "bold", size = 14))
ggsave("../results/TPC_f1.pdf", plot = pf1)



#### 2.  "Stethorus punctillum" ####
d2 <- subset(data, data$species == "Stethorus punctillum")
d2 <- gdata::drop.levels(d2)
unique(d2$Variable) # "Adult Mortality Rate (z)" [no time information, deleted] ; "Development Time (a)" 


#### a
a2 <- subset(d2, d2$Variable == "Development Time (a)")
a2 <- subset(a2, a2$originaltraitname == "Development Rate")
a2 <- subset(a2, a2$traitvalue != "0")

a2$stdvalue <- 1/a2$traitvalue
a2$stdtemp <- round(a2$temp) # get the  integer
a2 <- gdata::drop.levels(a2) 
stage2 <- unique(a2$stage) #get stage ranges: "Egg"  "1st instar" "2nd instar" "3rd instar" "4th instar" "Pupae" 

#### (? )  [1]       NA       NA       NA       NA       NA       NA       NA       NA       NA
# 1659.476
stdtotalvalue <- c() 
unitemp2 <- unique(a2$stdtemp) # 14 16 20 24 28 30 32 34 36 12
for (i in length(unitemp2) ){
  df <- a2[which(a2$stdtemp == unitemp2[i]),]
  stdtotalvalue[i] <- sum(df$stdvalue)
}

# write it 
stdtotalvalue <- c(   77.28209,   49.08416,   26.13132,   17.13008,   12.72742,   12.44748,   11.77101, 12.98787, 1412.37119, 1659.47587)
total2 <- data.frame(unitemp2, stdtotalvalue)


# plot
f_a2 <- ggplot(total2, aes(x = unitemp2, y = stdtotalvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_a2.pdf", plot = f_a2)


#### put together ---- only one trait
pf2 <- ggarrange(f_a2,labels = c("a"))
pf2 <- annotate_figure(pf2, top = text_grob("2. Stethorus punctillum", color = "red", face = "bold", size = 14))
ggsave("../results/TPC_f2.pdf", plot = pf2)


#### 3. Sitona discoideus #######################################################################################################
d3 <- subset(data, data$species == "Sitona discoideus")
d3 <- gdata::drop.levels(d3)
# check Variables
unique(d3$Variable) # only "Development Time (a)" for egg

#### 1)  a3
f_a3 <- ggplot(d3, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_a3.pdf", plot = f_a3)


#### put together ---- only one trait
pf3 <- ggarrange(f_a3,labels = c("a"))
pf3 <- annotate_figure(pf3, top = text_grob("3. Sitona discoideus", color = "red", face = "bold", size = 14))
ggsave("../results/TPC_f3.pdf", plot = pf3)


#-----------------------------------------------------------------------------------------------------------------#

#------------------------------------------------"Diptera"-----------------------------------------------------------------#
#### 4.  "Aedes aegypti" #######################################################################################################
d4 <- subset(data, data$species == "Aedes aegypti")
d4 <- gdata::drop.levels(d4)
# check Variables
unique(d4$Variable)
# "Fecundity"     "Adult Mortality Rate (z)"

####  1) a4 no information
####  2) z4
z4 <- subset(d4, d4$Variable == "Adult Mortality Rate (z)")
z4 <- gdata::drop.levels(z4)
unique(z4$originaltraitname) # "Mortality Rate" "Survival Time" 
z4$stdunit <- "1/day"

z4a <- subset(z4, z4$originaltraitname == "Mortality Rate")
z4a <- gdata::drop.levels(z4a)
z4a$stdvalue <- z4a$traitvalue

z4b <- subset(z4, z4$originaltraitname == "Survival Time")
z4b <- gdata::drop.levels(z4b)
z4b$stdvalue <- 1/z4b$traitvalue

z4 <- rbind(z4a, z4b)

# plot
f_z4 <- ggplot(z4, aes(x=temp, y = stdvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Adult Mortality Rate (z)", 
       x = "Temperature (\u00B0C) ", 
       y = "Adult Mortality Rate (1/day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_z4.pdf", plot = f_z4)


#### 3) zj
# no time information of juvenile, so delete this trait

#### 4）Fecundity b 
b4 <- subset(d4, d4$Variable == "Fecundity")
b4 <- gdata::drop.levels(b4)

# only one original name : Oviposition Rate

## plot b ~ time # no time information

## plot bpk ~ temperature
f_b4 <- ggplot(b4, aes(x = temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Peak Fecundity Rate (b)", 
       x = "Temperature (\u00B0C)", 
       y = "Fecundity (Eggs/female/day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_b4.pdf", plot = f_b4)


## 5） plot k ~temperature
# no time information

#### put together
p4 <- plot_grid(f_z4,f_b4,labels = c("z","b"),scales = "free")  # ncol=2, nrow=2, 
p4 <- annotate_figure(p4, top = text_grob("4. Aedes aegypti", color = "red", face = "bold", size = 14))
ggsave("../results/TPC_f4.pdf", plot = p4)


### 5. Aedes albopictus ###############################################################################################
d5 <- subset(data, data$species == "Aedes albopictus")
d5 <- gdata::drop.levels(d5)
unique(d5$Variable) # "Development Time (a)"， "Fecundity"， "Adult Mortality Rate (z)"

#### 1)  a
a5 <- subset(d5, d5$Variable == "Development Time (a)")
a5 <- gdata::drop.levels(a5) 

a5$stdvalue <- 1/a5$traitvalue

a_ave5 <- c()
temp5 <- unique(a5$temp)  # get temperature ranges: 15 20 25 30 35

for (i in 1: length(temp5)) {
  df <- a5[which(a5$temp == temp5[i]),]
  a_ave5[i] <- ave(df$stdvalue)
}
a5 <- data.frame(a_ave5, temp5)

f_a5 <- ggplot(a5, aes(x=temp5, y = a_ave5)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_a5.pdf", plot = f_a5)

#### 2) z 3) zj
z5 <- subset(d5, d5$Variable == "Adult Mortality Rate (z)")
z5 <- gdata::drop.levels(z5)
unique(z5$unit)
## no time information


#### 4) "Fecundity", b
b5 <- subset(d5, d5$Variable == "Fecundity")
b5 <- gdata::drop.levels(b5)
unique(b5$unit) # "eggs per female per cycle"

bpk5 <- c()
temp5 <- unique(b5$temp)  # get temperature ranges: 20 25 30 35
for (i in 1: length(temp5)) {
  df <- b5[which(b5$temp == temp5[i]),]
  bpk5[i] = max(df$traitvalue)
}
bpk5 <- data.frame(bpk5, temp5)

f_b5 <- ggplot(bpk5, aes(x = temp5, y = bpk5)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Peak Fecundity Rate (b)", 
       x = "Temperature (\u00B0C)", 
       y = "Fecundity (Eggs/female/day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_b5.pdf", plot = f_b5)

#### 5) k no time information

#### put together
pf5 <- ggarrange(f_a5,f_b5, labels = c("a","b"), scales = "free")
pf5 <- annotate_figure(pf5, top = text_grob("5. Aedes albopictus", color = "red", face = "bold", size = 14))
ggsave("../results/TPC_f5.pdf", plot = pf5)


### 6. Aedes camptorhynchus ###############################################################################################
d6 <- subset(data, data$species == "Aedes camptorhynchus")
d6 <- gdata::drop.levels(d6)
unique(d6$Variable) # [1] "Development Time (a)"         "Juvenile Mortality Rate (zJ)", "Fecundity" 

#### 1) a
a6 <- subset(d6, d6$Variable == "Development Time (a)")
a6 <- gdata::drop.levels(a6) 

a_ave6 <- c()
temp6 <- unique(a6$temp)  # get temperature ranges: 15 20 25 30 35

for (i in 1: length(temp6)) {
  df <- a6[which(a6$temp == temp6[i]),]
  if (df$temp == 25){
    a_ave6[3] <- unique(ave(a6$traitvalue))
  }
  else {
    a_ave6[i] <- df$traitvalue
  }
}
a6 <- data.frame(a_ave6, temp6)

# plot
f_a6 <- ggplot(a6, aes(x=temp6, y = a_ave6)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_a6.pdf", plot = f_a6)


#### 3) zj
zj6 <- subset(d6, d6$Variable == "Juvenile Mortality Rate (zJ)")
zj6 <- gdata::drop.levels(zj6)
unique(zj6$unit)

# no time information

#### 4) b
b6 <- subset(d6, d6$Variable == "Fecundity")
b6 <- gdata::drop.levels(b6)
b6$stdvalue <- (b6$traitvalue)/7

#### 5) k no time information

#### put together
pf6 <- ggarrange(f_a6,labels = c("a"))
pf6 <- annotate_figure(pf6, top = text_grob("6. Aedes camptorhynchus", color = "red", face = "bold", size = 14))
ggsave("../results/TPC_f6.pdf", plot = pf6)



### 7. Bactrocera correcta ###############################################################################################
d7 <- subset(data, data$species == "Bactrocera correcta")
d7 <- gdata::drop.levels(d7)
unique(d7$Variable) #"Development Time (a)"

### 1) a
a7 <- d7
a_ave7 <- c()
temp7 <- unique(a7$temp)  # get temperature ranges: 15 20 25 30 35

for (i in 1: length(temp7)) {
  df <- a7[which(a7$temp == temp7[i]),]
  a_ave7[i] <- ave(df$traitvalue)
}
a7 <- data.frame(a_ave7, temp7)

f_a7 <- ggplot(a1, aes(x=temp7, y = a_ave7)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_a7.pdf", plot = f_a7)

# no other traits

#### put together
pf7 <- ggarrange(f_a7,labels = c("a"))
pf7 <- annotate_figure(pf7, top = text_grob("7. Bactrocera correcta", color = "red", face = "bold", size = 14))
ggsave("../results/TPC_f7.pdf", plot = pf7)



### 8. Aedes notoscriptus ####
d8 <- subset(data, data$species == "Aedes notoscriptus")
d8 <- gdata::drop.levels(d8)
unique(d8$Variable) #"Juvenile Mortality Rate (zJ)" "Development Time (a)"

# 1) a 
a8 <- subset(d8, d8$Variable == "Development Time (a)")
a8 <- gdata::drop.levels(a8)

a_ave8 <- c()
temp8 <- unique(a8$temp)  # get temperature ranges: 15 18 25 29

for (i in 1: length(temp8)) {
  df <- a8[which(a8$temp == temp8[i]),]
  a_ave8[i] <- ave(df$traitvalue)
}
a8 <- data.frame(a_ave8, temp8)
a8 <- gdata::drop.levels(a8)

# a8 <- data.frame(a_ave8 = c(32.15, 28.50, 18.85,  7.20), temp8 = c( 15,18, 25, 29))

f_a8 <- ggplot(a8, aes(x=temp8, y = a_ave8)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_a8.pdf", plot = f_a8)

### "Juvenile Mortality Rate (zJ)"
zj8 <- subset(d8, d8$Variable == "Juvenile Mortality Rate (zJ)")
zj8 <- subset(zj8, zj8$traitvalue != 0)
zj8 <- gdata::drop.levels(zj8)
zj8$stdvalue <- 1/zj8$traitvalue

zj_ave8 <- c()
temp_zj8 <- unique(zj8$temp)  # get temperature ranges: 15 18 25 29 35


for (i in 1: length(temp_zj8)) {
  df <- zj8[which(zj8$temp == temp_zj8[i]),]
  zj_ave8[i] <- ave(df$stdvalue)
}
zj8 <- data.frame(zj_ave8, temp_zj8)
zj8 <- gdata::drop.levels(zj8)

f_zj8 <- ggplot(zj8, aes(x=temp_zj8, y = zj_ave8)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Juvenile Mortality Rate (zJ)", 
       x = "Temperature (\u00B0C) ", 
       y = "Juvenile Mortality Rate (1/day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_zj8.pdf", plot = f_zj8)

#### put together
pf8 <- ggarrange(f_a8,f_zj8,labels = c("a","zJ"), scales = "free")
pf8 <- annotate_figure(pf8, top = text_grob("8. Aedes notoscriptus", color = "red", face = "bold", size = 14))
ggsave("../results/TPC_f8.pdf", plot = pf8)


#### 9. Culex annulirostris ####
d9 <- subset(data, data$species == "Culex annulirostris")
d9 <- gdata::drop.levels(d9)
unique(d9$Variable)  # [1] "Juvenile Mortality Rate (zJ)" "Development Time (a)"         "Adult Mortality Rate (z)"    [4] "Fecundity",  NA 

d9 <- subset(d9, d9$Variable != "NA")
d9 <- gdata::drop.levels(d9)
unique(d9$Variable) # # [1] "Juvenile Mortality Rate (zJ)","Development Time (a)","Adult Mortality Rate (z)","Fecundity"

#### 1)  a
a9 <- subset(d9, d9$Variable == "Development Time (a)")
a9 <- gdata::drop.levels(a9) 
a9 <- subset(a9, a9$stage == "egg + larvae + pupae")

f_a9 <- ggplot(a9, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_a9.pdf", plot = f_a9)

#### 2)  z1 "Adult Mortality Rate (z)"
z9 <- subset(d9, d9$Variable == "Adult Mortality Rate (z)")
unique(z9$unit)
z9 <- subset(z9, z9$unit != "proportion") # no time information
z9 <- gdata::drop.levels(z9)

z9 <- subset(z9, z9$originaltraitname == "Adult longevity (female, bloodfed)") #
z9$stdvalue <- 1/z9$traitvalue

f_z9 <- ggplot(z9, aes(x=temp, y = stdvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Adult Mortality Rate (z)", 
       x = "Temperature (\u00B0C) ", 
       y = "Adult Mortality Rate (1/day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_z9.pdf", plot = f_z9)

# "Juvenile Mortality Rate (zJ)"
zj9 <- subset(d9, d9$Variable == "Juvenile Mortality Rate (zJ)")
# no time information

# "Fecundity"
b9 <- subset(d9, d9$Variable == "Fecundity")
b9 <- gdata::drop.levels(b9)

# no time information
## plot b ~ time
bpk9 <- c()
temp9 <- unique(b9$temp)  # get temperature ranges: 20 25 30

for (i in 1: length(temp9)) {
  df <- b9[which(b9$temp == temp9[i]),]
  bpk9[i] = max(df$traitvalue)
  
}
bpk9 <- data.frame(bpk9, temp9)

f_b9 <- ggplot(bpk9, aes(x = temp9, y = bpk9)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Peak Fecundity Rate (b)", 
       x = "Temperature (\u00B0C)", 
       y = "Fecundity (Eggs/female/day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_b9.pdf", plot = f_b9)

#### put together
pf9 <- ggarrange(f_a9,f_z9,f_b9,labels = c("a","z","b"))
pf9 <- annotate_figure(pf9, top = text_grob("9. Culex annulirostris", color = "red", face = "bold", size = 14))
ggsave("../results/TPC_f9.pdf", plot = pf9)


#-----------------------------------------------------------------------------------------------------------------#

#------------------------------------------------"Hemiptera"-----------------------------------------------------------------#

### 10. Aphis gossypii
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
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_a10.pdf", plot = f_a10)



### 2) z      "Adult Mortality Rate (z)" 
z10 <- subset(d10, d10$originaltraitname == "Longevity")
z10 <- gdata::drop.levels(z10)
z10$stdvalue <- 1/z10$traitvalue
z10$stdunit <- "1/day"

f_z10 <- ggplot(z10, aes(x=temp, y = stdvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Adult Mortality Rate (z)", 
       x = "Temperature (\u00B0C) ", 
       y = "Adult Mortality Rate (1/day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_z10.pdf", plot = f_z10)


### 3) zJ 
zj10 <- subset(d10,d10$stage == "juvenile")
zj10 <- gdata::drop.levels(zj10)

f_zj10 <- ggplot(z10, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Juvenile Mortality Rate (z)", 
       x = "Temperature (\u00B0C) ", 
       y = "Juvenile Mortality Rate (1/day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_zj10.pdf", plot = f_zj10)

### 4) b      "Fecundity" 
b10 <- subset(d10, d10$Variable == "Fecundity")
b10 <- gdata::drop.levels(b10)

bpk10 = c()
temp10 <- unique(b10$temp)
for (i in 1: length(temp10)) {
  bpk <- b10[which(b10$temp == temp10[i]),]
  bpk10[i] = max(bpk$traitvalue)
}
bpk10 <- data.frame(bpk10, temp10)

f_b10 <- ggplot(bpk10, aes(x = temp10, y = bpk10)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Peak Fecundity Rate (b)", 
       x = "Temperature (\u00B0C)", 
       y = "Fecundity (Eggs/female/day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_b10.pdf", plot = f_b10)

#### put together
pf10 <- ggarrange(f_a10,f_z10,f_zj10, f_b10,labels = c("a","z","zj","b"))
pf10 <- annotate_figure(pf10, top = text_grob("10. Aphis gossypii", color = "red", face = "bold", size = 14))
ggsave("../results/TPC_f10.pdf", plot = pf10)



#### 11. Corythucha ciliata
d11 <- subset(data, data$species == "Corythucha ciliata")
d11 <- gdata::drop.levels(d11)
unique(d11$Variable) #"Development Time (a)"，"Fecundity"， "Adult Mortality Rate (z)"


### 1) a  "Development Time (a)"   
a11 <- subset(d11, d11$Variable == "Development Time (a)")
a11 <- gdata::drop.levels(a11) 

unique(a11$unit) # "100/day" "days" 

a11a <- subset(a11, a11$unit == "100/day")
a11a <- gdata::drop.levels(a11a) 
unique(a11a$stage)
a11a <- subset(a11a, a11a$stage == "Egg-to-adult")
a11a$stdvalue <- 100/(a11a$traitvalue)

a11b <- subset(a11, a11$unit == "days")
unique(a11b$stage)
a11b <- subset(a11b, a11b$stage == "Egg-to-adult")
a11b$stdvalue <- a11b$traitvalue

a11 <- rbind(a11a, a11b)

f_a11 <- ggplot(a11, aes(x=temp, y = stdvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_a11.pdf", plot = f_a11)


# 2) "Adult Mortality Rate (z)"
z11 <- subset(d11, d11$Variable == "Adult Mortality Rate (z)")
unique(z11$unit) # days； %, no time information, deleted.

z11 <- subset(z11, z11$originaltraitname == "Longevity")
z11 <- gdata::drop.levels(z11)
z11$stdvalue <- 1/z11$traitvalue


f_z11 <- ggplot(z11, aes(x=temp, y = stdvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Adult Mortality Rate (z)", 
       x = "Temperature (\u00B0C) ", 
       y = "Adult Mortality Rate (1/day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_z11.pdf", plot = f_z11)

# "Fecundity"
b11 <- subset(d11, d11$Variable == "Fecundity")
b11 <- gdata::drop.levels(b11)

f_b11 <- ggplot(b11, aes(x = temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Peak Fecundity Rate (b)", 
       x = "Temperature (\u00B0C)", 
       y = "Fecundity (Eggs/female/day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_b11.pdf", plot = f_b11)

#### put together
pf11 <- ggarrange(f_a11,f_z11,f_b11,labels = c("a","z","b"))
pf11 <- annotate_figure(pf11, top = text_grob("11.Corythucha ciliata", color = "red", face = "bold", size = 14))
ggsave("../results/TPC_f11.pdf", plot = pf11)


#### 12. Planococcus citri
d12 <- subset(data, data$species == "Planococcus citri")
d12 <- gdata::drop.levels(d12)
unique(d12$Variable) # "Development Time (a)"     "Adult Mortality Rate (z)"

# 1) a 
a12 <- subset(d12, d12$Variable == "Development Time (a)")
a12 <- gdata::drop.levels(a12)
a12 <- subset(a12, a12$stage == "adult")

f_a12 <- ggplot(a12, aes(x = temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_a12.pdf", plot = f_a12)

# 2) z
z12 <- subset(d12, d12$Variable == "Adult Mortality Rate (z)")
z12 <- gdata::drop.levels(z12)

z12 <- subset(z12, z12$stage == "adult (female)")
z12$stdvalue <- 1/z12$traitvalue

f_z12 <- ggplot(z12, aes(x=temp, y = stdvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Adult Mortality Rate (z)", 
       x = "Temperature (\u00B0C) ", 
       y = "Adult Mortality Rate (1/day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_z12.pdf", plot = f_z12)

#### put together
p12 <- plot_grid(f_a12,f_z12,labels = c("a","z"),scales = "free")  # ncol=2, nrow=2, 
p12 <- annotate_figure(p12, top = text_grob("4. Aedes aegypti", color = "red", face = "bold", size = 14))
ggsave("../results/TPC_f12.pdf", plot = p12)


#-----------------------------------------------------------------------------------------------------------------#

#------------------------------------------------"Hymenoptera"-----------------------------------------------------------------#
### 13. Euplectrus ronnai
d13 <- subset(data, data$species == "Euplectrus ronnai")
d13 <- gdata::drop.levels(d13)
unique(d13$Variable) # "Adult Mortality Rate (z)" "Development Time (a)"    

#### 1) a 
a13 <- subset(d13, d13$Variable == "Development Time (a)")
a13 <- gdata::drop.levels(a13) 
unique(a13$stage) #larvae /pupae


a_sum13 <- c()
temp13 <- unique(a13$temp)  # get temperature ranges: 15 20 25 30 35

for (i in 1: length(temp13)) {
  df <- a13[which(a13$temp == temp13[i]),]
  a_sum13[i] <- sum(df$traitvalue)
}
a13 <- data.frame(a_sum13, temp13)

f_a13 <- ggplot(a13, aes(x=temp13, y = a_sum13)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_a13.pdf", plot = f_a13)

#### 2) z
z13 <- subset(d13, d13$Variable == "Adult Mortality Rate (z)")
z13 <- gdata::drop.levels(z13)  # stage: juvenile

zj13 <-z13
f_zj13 <- ggplot(zj13, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Juvenile Mortality Rate (zJ)", 
       x = "Temperature (\u00B0C) ", 
       y = "Juvenile Mortality Rate (1/day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_zj13.pdf", plot = f_zj13)


#### put together
pf13 <- ggarrange(f_a13,f_zj13,labels = c("a","zJ"), scales = "free")
pf13 <- annotate_figure(pf13, top = text_grob("13. Euplectrus ronnai", color = "red", face = "bold", size = 14))
ggsave("../results/TPC_f13.pdf", plot = pf13)


#### 14. Glyptapanteles muesebecki
d14 <- subset(data, data$species == "Glyptapanteles muesebecki")
d14 <- gdata::drop.levels(d14)
unique(d14$Variable) #  "Development Time (a)"    

a14 <- d14
a_sum14 <- c()
temp14 <- unique(a14$temp)  # get temperature ranges: 15 20 25 30 35

for (i in 1: length(temp14)) {
  df <- a14[which(a14$temp == temp14[i]),]
  a_sum14[i] <- sum(df$traitvalue)
}
a14 <- data.frame(a_sum14, temp14)

f_a14 <- ggplot(a14, aes(x=temp14, y = a_sum14)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_a14.pdf", plot = f_a14)

#### put together
pf14 <- annotate_figure(f_a14, top = text_grob("14. Glyptapanteles muesebecki", color = "red", face = "bold", size = 14))
ggsave("../results/TPC_f14.pdf", plot = pf14)


#### 15. Macrocentrus iridescens
d15 <- subset(data, data$species == "Macrocentrus iridescens")
d15 <- gdata::drop.levels(d15)
unique(d15$Variable) #  "Development Time (a)"    

a15 <- d15
a15$stdvalue <- 1/a15$traitvalue

f_a15 <- ggplot(a15, aes(x=temp, y = stdvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_a15.pdf", plot = f_a15)


#### put together
pf15 <- annotate_figure(f_a15, top = text_grob("15. Macrocentrus iridescens", color = "red", face = "bold", size = 14))
ggsave("../results/TPC_f15.pdf", plot = pf15)



#### 16. Telenomus chrysopae
d16 <- subset(data, data$species == "Telenomus chrysopae")
d16 <- gdata::drop.levels(d16)
unique(d16$Variable) # "Adult Mortality Rate (z)" "Development Time (a)"    

#### 1) a 
a16 <- subset(d16, d16$Variable == "Development Time (a)")
a16 <- gdata::drop.levels(a16) 
unique(a16$stage) #larvae 

f_a16 <- ggplot(a16, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_a16.pdf", plot = f_a16)


#### 3)zJ
z16 <- subset(d16, d16$Variable == "Adult Mortality Rate (z)")
z16 <- gdata::drop.levels(z16)  # stage: juvenile

zj16 <-z16
f_zj16 <- ggplot(zj16, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Juvenile Mortality Rate (zJ)", 
       x = "Temperature (\u00B0C) ", 
       y = "Juvenile Mortality Rate (1/day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_zj16.pdf", plot = f_zj16)

#### put together
pf16 <- ggarrange(f_a16,f_zj16,labels = c("a","zJ"), scales = "free")
pf16 <- annotate_figure(pf16, top = text_grob("16. Euplectrus ronnai", color = "red", face = "bold", size = 14))
ggsave("../results/TPC_f16.pdf", plot = pf16)



#### 17. Telenomus isis
d17 <- subset(data, data$species == "Telenomus isis")
d17 <- gdata::drop.levels(d17)
unique(d17$Variable) #"Development Time (a)"     "Adult Mortality Rate (z)"


# 1) a       "Development Time (a)"   
a17 <- subset(d17, d17$Variable == "Development Time (a)")
a17 <- gdata::drop.levels(a17) 
a17$stdvalue <- 1/a17$traitvalue

f_a17 <- ggplot(a17, aes(x=temp, y = stdvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_a17.pdf", plot = f_a17)


#  2) z      "Adult Mortality Rate (z)" 
z17 <- subset(d17, d17$Variable == "Adult Mortality Rate (z)")
z17 <- gdata::drop.levels(z17)

unique(z17$stage) # "juvenile"       "adult (female)"
z17 <- subset(z17,z17$stage == "adult (female)")
z17$stdvalue <- 1/ z17$traitvalue

f_z17 <- ggplot(z17, aes(x=temp, y = stdvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Adult Mortality Rate (z)", 
       x = "Temperature (\u00B0C) ", 
       y = "Adult Mortality Rate (1/day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_z17.pdf", plot = f_z17)


#  3) zj 
zj17 <- subset(d17, d17$Variable == "Adult Mortality Rate (z)")
zj17 <- gdata::drop.levels(zj17)

zj17 <- subset(zj17,zj17$stage == "juvenile")

f_zj17 <- ggplot(zj17, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Juvenile Mortality Rate (zJ)", 
       x = "Temperature (\u00B0C) ", 
       y = "Juvenile Mortality Rate (1/day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_zj17.pdf", plot = f_zj17)

#### put together
pf17 <- ggarrange(f_a17,f_z17,f_zj17,labels = c("a","z","zJ"))
pf17 <- annotate_figure(pf17, top = text_grob("17. Telenomus isis", color = "red", face = "bold", size = 14))
ggsave("../results/TPC_f17.pdf", plot = pf17)


# Saving 7 x 7 in image


#### 18. Telenomus lobatus
d18 <- subset(data, data$species == "Telenomus lobatus")
d18 <- gdata::drop.levels(d18)
unique(d18$Variable) #"Development Time (a)"     "Adult Mortality Rate (z)"

# 1）a
a18 <- subset(d18, d18$Variable == "Development Time (a)")
a18 <- gdata::drop.levels(a18) 

f_a18 <- ggplot(a18, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_a18.pdf", plot = f_a18)



# 2）z
z18 <- subset(d18, d18$Variable == "Adult Mortality Rate (z)")
z18 <- gdata::drop.levels(z18) ## juvenile

# 3) zj
zj18 <- z18

f_zj18 <- ggplot(zj18, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Juvenile Mortality Rate (zJ)", 
       x = "Temperature (\u00B0C) ", 
       y = "Juvenile Mortality Rate (1/day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_zj18.pdf", plot = f_zj18)

#### put together
pf18 <- ggarrange(f_a18,f_zj18,labels = c("a","zJ"), scale = "free")
pf18 <- annotate_figure(pf18, top = text_grob("18. Telenomus lobatus", color = "red", face = "bold", size = 14))
ggsave("../results/TPC_f18.pdf", plot = pf18)



#### 19. Theocolax elegans
d19 <- subset(data, data$species == "Theocolax elegans")
d19 <- gdata::drop.levels(d19)
unique(d19$Variable) #"Development Time (a)"     "Adult Mortality Rate (z)"

# 1）a
a19 <- subset(d19, d19$Variable == "Development Time (a)")
a19 <- gdata::drop.levels(a19) 


f_a19 <- ggplot(a19, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_a19.pdf", plot = f_a19)


# 2）z
z19 <- subset(d19, d19$Variable == "Adult Mortality Rate (z)")
z19 <- gdata::drop.levels(z19) 
z19 <- subset(z19, z19$stage == "adult (female)")
z19$stdvalue <- 1/z19$traitvalue

f_z19 <- ggplot(z19, aes(x=temp, y = stdvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Adult Mortality Rate (z)", 
       x = "Temperature (\u00B0C) ", 
       y = "Adult Mortality Rate (1/day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_z19.pdf", plot = f_z19)

#### put together
pf19 <- ggarrange(f_a19,f_z19,labels = c("a","z"), scale = "free")
pf19 <- annotate_figure(pf19, top = text_grob("19. Theocolax elegans", color = "red", face = "bold", size = 14))
ggsave("../results/TPC_f19.pdf", plot = pf19)


#### 20. Trichogramma bruni
d20 <- subset(data, data$species == "Trichogramma bruni")
d20 <- gdata::drop.levels(d20)
unique(d20$Variable) #"Development Time (a)"   

# 1）a
a20 <- subset(d20, d20$Variable == "Development Time (a)")
a20 <- gdata::drop.levels(a20) 


f_a20 <- ggplot(a20, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_a20.pdf", plot = f_a20)

#### put together
pf20 <- ggarrange(f_a20,labels = c("a"))
pf20 <- annotate_figure(pf20, top = text_grob("20. Trichogramma bruni", color = "red", face = "bold", size = 14))
ggsave("../results/TPC_f20.pdf", plot = pf20)


#### 21. Trichogramma sp. nr. Lutea
d21 <- subset(data, data$species == "Trichogramma sp. nr. Lutea")
d21 <- gdata::drop.levels(d21)
unique(d21$Variable) #"Development Time (a)"   

# 1）a
a21 <- subset(d21, d21$Variable == "Development Time (a)")
a21 <- gdata::drop.levels(a21) 


f_a21 <- ggplot(a21, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_a21.pdf", plot = f_a21)

#### put together
pf21 <- ggarrange(f_a21,labels = c("a"))
pf21 <- annotate_figure(pf21, top = text_grob("21. Trichogramma sp. nr. Lutea", color = "red", face = "bold", size = 14))
ggsave("../results/TPC_f21.pdf", plot = pf21)


#### 22. Trichogramma sp. nr. Mwanzai
d22 <- subset(data, data$species == "Trichogramma sp. nr. Mwanzai")
d22 <- gdata::drop.levels(d22)
unique(d22$Variable) #"Development Time (a)"   

# 1）a
a22 <- subset(d22, d22$Variable == "Development Time (a)")
a22 <- gdata::drop.levels(a22) 

f_a22 <- ggplot(a22, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_a22.pdf", plot = f_a22)

#### put together
pf22 <- ggarrange(f_a22,labels = c("a"))
pf22 <- annotate_figure(pf22, top = text_grob("22. Trichogramma sp. nr. Mwanzai", color = "red", face = "bold", size = 14))
ggsave("../results/TPC_f22.pdf", plot = pf22)



#-----------------------------------------------------------------------------------------------------------------#

#------------------------------------------------"Lepidoptera"-----------------------------------------------------------------#

#### 23. Cydia pomonella
d23 <- subset(data, data$species == "Cydia pomonella")
d23 <- gdata::drop.levels(d23)
unique(d23$Variable) #"Development Time (a)"   

# 1）a
a23 <- subset(d23, d23$Variable == "Development Time (a)")
a23 <- gdata::drop.levels(a23) 

f_a23 <- ggplot(a23, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_a23.pdf", plot = f_a23)

#### put together
pf23 <- ggarrange(f_a23,labels = c("a"))
pf23 <- annotate_figure(pf23, top = text_grob("23. Cydia pomonella", color = "red", face = "bold", size = 14))
ggsave("../results/TPC_f23.pdf", plot = pf23)


#### 24. Lepinotus reticulatus
d24 <- subset(data, data$species == "Lepinotus reticulatus")
d24 <- gdata::drop.levels(d24)
unique(d24$Variable) #"Development Time (a)" ， "Adult Mortality Rate (z)"

# 1）a
a24 <- subset(d24, d24$Variable == "Development Time (a)")
a24 <- gdata::drop.levels(a24) 

a24 <- subset(a24, a24$stage != "")
a_sum24 <- c()
temp24 <- unique(a24$temp)  # get temperature ranges: 22.5 25.0 27.5 30.0 32.5 35.0

for (i in 1: length(temp24)) {
  df <- a24[which(a24$temp == temp24[i]),]
  a_sum24[i] <- sum(df$traitvalue)
}
a24 <- data.frame(a_sum24, temp24)

f_a24 <- ggplot(a24, aes(x=temp24, y = a_sum24)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_a24.pdf", plot = f_a24)

#### put together
pf24 <- ggarrange(f_a24,labels = c("a"))
pf24 <- annotate_figure(pf24, top = text_grob("24. Lepinotus reticulatus", color = "red", face = "bold", size = 14))
ggsave("../results/TPC_f24.pdf", plot = pf24)


# 2) z
z24 <- subset(d24, d24$Variable == "Adult Mortality Rate (z)")
z24 <- gdata::drop.levels(z24)  # stage: juvenile

f_z24 <- ggplot(z24, aes(x=temp, y = traitvalue)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Juvenile Mortality Rate (zJ)", 
       x = "Temperature (\u00B0C) ", 
       y = "Juvenile Mortality Rate (1/day)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_z24.pdf", plot = f_z24)

#### put together
pf24 <- ggarrange(f_a24,f_z24,labels = c("a","z"), scales = "free")
pf24 <- annotate_figure(pf24, top = text_grob("24. Lepinotus reticulatus", color = "red", face = "bold", size = 14))
ggsave("../results/TPC_f24.pdf", plot = pf24)


#### 25. Tetranychus mcdanieli
d25 <- subset(data, data$species == "Tetranychus mcdanieli")
d25 <- gdata::drop.levels(d25)
unique(d25$Variable) #"Development Time (a)" ， "Adult Mortality Rate (z)"

# 1) a
a25 <- subset(d25, d25$Variable == "Development Time (a)")
a25 <- gdata::drop.levels(a25) 
unique(a25$stage) # "Egg"        "Larvae"     "Protonymph" "Deutonymph"
a25$stdtemp <- round(a25$temp) # 12 14 16 20 24 28 30 32 34 36 38
a25$stdvalue <- 1/a25$traitvalue
  
a_sum25 <- c()
temp25 <- unique(a25$stdtemp) 

for (i in 1: length(temp25)) {
  df <- a25[which(a25$stdtemp == temp25[i]),]
  a_sum25[i] <- sum(df$stdvalue)
}
a25 <- data.frame(a_sum25, temp25)
a25 <- subset(a25, a25$a_sum25 != "Inf")
a25 <- gdata::drop.levels(a25) 

f_a25 <- ggplot(a25, aes(x=temp25, y = a_sum25)) + theme_bw()+
  geom_point(show.legend = FALSE,color = 'black',size = 3) +
  labs(title = "TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)")+
  geom_smooth(method = "lm", formula = y ~ poly(x,2),color='blue',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f1,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right", label.y = 0.9, label.x = 0.1,size = 2) +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))+
  geom_smooth(method = "lm", formula = y ~ poly(x,3),color='red',size = 1,se = T)+  ## quadratic fit
  stat_poly_eq(formula = f2,
               aes(label = paste(..eq.label.., ..rr.label..,..AIC.label.., sep = "~~~")),
               parse = TRUE,label.x.npc = "right",label.y = 0.85, label.x = 0.1,size = 2)  # label.y.npc = "bottom"

ggsave("../results/TPC_fit_a25.pdf", plot = f_a25)

#### put together
pf25 <- ggarrange(f_a25,labels = c("a"))
pf25 <- annotate_figure(pf25, top = text_grob("25. Tetranychus mcdanieli", color = "red", face = "bold", size = 14))
ggsave("../results/TPC_f25.pdf", plot = pf25)



######################## combine all figures together
pdf_combine(c("../results/TPC_f1.pdf",
              "../results/TPC_f2.pdf",
              "../results/TPC_f3.pdf",
              "../results/TPC_f4.pdf",
              "../results/TPC_f5.pdf",
              "../results/TPC_f6.pdf",
              "../results/TPC_f7.pdf",
              "../results/TPC_f8.pdf",
              "../results/TPC_f9.pdf",
              "../results/TPC_f10.pdf",
              "../results/TPC_f11.pdf",
              "../results/TPC_f12.pdf",
              "../results/TPC_f13.pdf",
              "../results/TPC_f14.pdf",
              "../results/TPC_f15.pdf",
              "../results/TPC_f16.pdf",
              "../results/TPC_f17.pdf",
              "../results/TPC_f18.pdf",
              "../results/TPC_f19.pdf",
              "../results/TPC_f20.pdf",
              "../results/TPC_f21.pdf",
              "../results/TPC_f22.pdf",
              "../results/TPC_f23.pdf",
              "../results/TPC_f24.pdf",
              "../results/TPC_f25.pdf"), 
            output = "../results/3.TPC_all species.pdf")





























