#!usr/bin/env R
###########################################################################################################
# Title: 3. Put all TPCs together in R
# MSc CMEE 
# Aug 2020 
# Author: YUAN ZHANG 
############################################################################################################

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

#--------- Load dataset and check levels --------#
data <- read.csv("../data/simple.csv")
unique((data$species))

b <- subset(data, data$Variable == "Fecundity")
b <- gdata::drop.levels(b)
unique((b$species))

#-----------------------------------------------------------------------------------------------------------------#

#------------------------------------------------"Coleoptera"-----------------------------------------------------------------#

#### 1.  "Anthonomus grandis" #######################################################################################################
d1 <- subset(data, data$species == "Anthonomus grandis")
d1 <- gdata::drop.levels(d1)
# check Variables
unique(d1$Variable)
# "Adult Mortality Rate (z)" "Development Time (a)"     "Fecundity"

#### a
a1 <- subset(d1, d1$Variable == "Development Time (a)")
a1 <- subset(a1, a1$stage == "Immature Stages")
a1 <- gdata::drop.levels(a1) 

p_a1 <- ggplot(a1, aes(x=temp, y = traitvalue))  + geom_point() + geom_smooth()+
  labs(title = "TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)") 
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))
ggsave("../results/TPC_a1.pdf", plot = p_a1)

####  z
z1 <- subset(d1, d1$Variable == "Adult Mortality Rate (z)")
z1 <- subset(z1, z1$originaltraitname == "Longevity")
z1 <- gdata::drop.levels(z1)
z1$stdvalue <- 1/z1$traitvalue

p_z1 <-  ggplot(z1, aes(x=temp, y = stdvalue))  + geom_point() + geom_smooth()+
  labs(title = "TPC of Adult Mortality Rate (z)", 
       x = "Temperature (\u00B0C) ", 
       y = "Adult Mortality Rate (1/day)") +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))
ggsave("../results/TPC_z1.pdf", plot = p_z1)

#### zj
zj1 <- subset(d1, d1$Variable == "Adult Mortality Rate (z)")
zj1 <- subset(zj1, zj1$time != "NA")
zj10 <- gdata::drop.levels(zj10)
# no time information of juvenile, so delete this trait

#### Fecundity b & k
b1 <- subset(b, b$species == "Anthonomus grandis")
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
p_bt <- ggplot(b1, aes(x = time, y = stdvalue)) + geom_point() + theme_bw() + 
  labs(title = "Fecundity change with time: 1.Anthonomus grandis", 
       x = "Time (days)", 
       y = "Fecundity (Eggs/female/day)")+
  theme(plot.title = element_text(hjust = 0.5,size = 10, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size= 10),
        axis.title.y=element_text(size= 10))

p_bt <- p_bt + facet_wrap(~ stdtemp, ncol = 2, scales = "free")+ geom_smooth() 
ggsave("../results/p_bt1.pdf", plot = p_bt)



## plot bpk ~ temperature
p_bpk1 <- ggplot(bpk1, aes(x = temp1, y = bpk1))+ geom_point() + geom_smooth()+
  labs(title = "TPC of Peak Fecundity Rate (b)", 
       x = "Temperature (\u00B0C)", 
       y = "Fecundity (Eggs/female/day)")+ 
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size= 8),
        axis.title.y=element_text(size= 8))
ggsave("../results/TPC_bpk1.pdf", plot = p_bpk1)

## plot k ~temperature
p_k1<- ggplot(bpk1, aes(x = temp1, y = k1))+ geom_point() + geom_smooth()+
  labs(title = "TPC of Fecundity Loss Rate (k)", 
       x = "Time (days)", 
       y = "Fecundity Loss Rate")+ 
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size= 8),
        axis.title.y=element_text(size= 8))
ggsave("../results/TPC_k1.pdf", plot = p_k1)


#### put together
p1 <- plot_grid(p_a1,p_z1,p_bpk1,p_k1,labels = c("a","z","b","k"), ncol=2,nrow=2)
p1 <- annotate_figure(p1, top = text_grob("1. Anthonomus grandis", color = "red", face = "bold", size = 14))
ggsave("../results/TPC_1.pdf", plot = p1)


#### 2.  "Stethorus punctillum" ###########################################################################################################
d2 <- subset(data, data$species == "Stethorus punctillum")
d2 <- gdata::drop.levels(d2)
unique(d2$Variable) # "Adult Mortality Rate (z)" "Development Time (a)" 

#### a
a2 <- subset(d2, d2$Variable == "Development Time (a)")
a2 <- subset(a2, a2$originaltraitname == "Development Rate")
a2 <- subset(a2, a2$traitvalue != "0")

a2$stdvalue <- 1/a2$traitvalue
a2 <- gdata::drop.levels(a2) 
stage2 <- unique(a2$stage) #get stage ranges: "Egg"  "1st instar" "2nd instar" "3rd instar" "4th instar" "Pupae" 

p_a2 <- ggplot(a2, aes(x = temp, y = stdvalue))  +   geom_point()+ theme_bw() + 
  labs(title = "TPC of Development Time (a)", 
       x = "Temperature (\u00B0C)", 
       y = "Development Time (days)")+
  theme(plot.title = element_text(hjust = 0.5,size = 10, face = "bold.italic"),
      axis.text=element_text(size=8,face = "bold"),
      axis.title.x=element_text(size= 10),
      axis.title.y=element_text(size= 10))

p_a2 <- p_a2 + facet_wrap(~ stage, ncol = 2, scales = "free")+ geom_smooth() 
p2 <- annotate_figure(p_a2, top = text_grob("2. Stethorus punctillum", color = "red", face = "bold", size = 14))
ggsave("../results/TPC_2.pdf", plot = p2)


#### 3. Sitona discoideus #######################################################################################################
d3 <- subset(data, data$species == "Sitona discoideus")
d3 <- gdata::drop.levels(d3)
# check Variables
unique(d3$Variable)


#-----------------------------------------------------------------------------------------------------------------#

#------------------------------------------------"Diptera"-----------------------------------------------------------------#
#### 4.  "Aedes aegypti" ####
d4 <- subset(data, data$species == "Aedes aegypti")
d4 <- gdata::drop.levels(d4)
# check Variables
unique(d4$Variable)
# "Fecundity"     "Adult Mortality Rate (z)"

####  z
z3 <- subset(d3, d3$Variable == "Adult Mortality Rate (z)")
z3 <- gdata::drop.levels(z3)
unique(z3$originaltraitname) # "Mortality Rate" "Survival Time" 
z3$stdunit <- "1/day"

z3a <- subset(z3, z3$originaltraitname == "Mortality Rate")
z3a <- gdata::drop.levels(z3a)
z3a$stdvalue <- z3a$traitvalue

z3b <- subset(z3, z3$originaltraitname == "Survival Time")
z3b <- gdata::drop.levels(z3b)
z3b$stdvalue <- 1/z3b$traitvalue

z3 <- rbind(z3a, z3b)

p_z3 <-  ggplot(z3, aes(x=temp, y = stdvalue))  + geom_point() + geom_smooth()+
  labs(title = "TPC of Adult Mortality Rate (z)", 
       x = "Temperature (\u00B0C) ", 
       y = "Adult Mortality Rate (1/day)") +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))
ggsave("../results/TPC_z3.pdf", plot = p_z3)

#### zj
# no time information of juvenile, so delete this trait

#### Fecundity b & k
b3 <- subset(d3, d3$Variable == "Fecundity")
b3 <- gdata::drop.levels(b3)

# only one original name : Oviposition Rate

## plot b ~ time # no time information

## plot bpk ~ temperature
p_bpk3 <- ggplot(b3, aes(x = temp, y = traitvalue))+ geom_point() + geom_smooth()+
  labs(title = "TPC of Peak Fecundity Rate (b)", 
       x = "Temperature (\u00B0C)", 
       y = "Fecundity (Eggs/female/day)")+ 
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size= 8),
        axis.title.y=element_text(size= 8))
ggsave("../results/TPC_bpk3.pdf", plot = p_bpk3)

## plot k ~temperature
# no time information


#### put together
p3 <- plot_grid(p_z3,p_bpk3,labels = c("z","b"),scales = "free")  # ncol=2, nrow=2, 
p3 <- annotate_figure(p3, top = text_grob("3. Aedes aegypti", color = "red", face = "bold", size = 14))
ggsave("../results/TPC_3.pdf", plot = p3)


#### 4. Aedes albopictus

#### 5. Aedes camptorhynchus
#### 6. Bactrocera correcta
#### 7. Aedes notoscriptus
#### 8. Culex annulirostris


##### "Hemiptera"  ##################
## 10) "Aphis gossypii"    
d10 <- subset(data, data$species == "Aphis gossypii")
d10 <- gdata::drop.levels(d10)

# a
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

p_a10 <- ggplot(a10, aes(x=temp, y = stdvalue))  + geom_point() + geom_smooth()+
  labs(title = "TPC of Development Time (a)", 
       x = "Temperature (\u00B0C) ", 
       y = "Development Time (days)") +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))
ggsave("../results/TPC_a10.pdf", plot = p_a10)

#  z
z10 <- subset(d10, d10$originaltraitname == "Longevity")
z10 <- gdata::drop.levels(z10)
z10$stdvalue <- 1/z10$traitvalue
z10$stdunit <- "1/day"

p_z10 <-  ggplot(z10, aes(x=temp, y = stdvalue))  + geom_point() + geom_smooth()+
  labs(title = "TPC of Adult Mortality Rate (z)", 
       x = "Temperature (\u00B0C) ", 
       y = "Adult Mortality Rate (1/day)") +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))
ggsave("../results/TPC_z10.pdf", plot = p_z10)

#  zj
zj10 <- subset(d10,d10$stage == "juvenile")
zj10 <- gdata::drop.levels(zj10)

p_zj10 <-  ggplot(zj10, aes(x=temp, y = traitvalue))  + geom_point() + geom_smooth()+
  labs(title = "TPC of Juvenile Mortality Rate (z_J)", 
       x = "Temperature (\u00B0C) ", 
       y = "Juvenile Mortality Rate (1/day)") +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size=8),
        axis.title.y=element_text(size=8))
ggsave("../results/TPC_zj10.pdf", plot= p_zj10)


# bpk
b10 <- subset(d10, d10$Variable == "Fecundity")
b10 <- gdata::drop.levels(b10)

bpk10 = c()
temp_10 <- unique(b10$temp)
for (i in 1: length(temp_10)) {
  bpk <- b10[which(b10$temp == temp_10[i]),]
  bpk10[i] = max(bpk$traitvalue)
}
bpk10 <- data.frame(bpk10, temp_10)

p_bpk10 <- ggplot(bpk10, aes(x=temp_10, y = bpk10))  + geom_point() + geom_smooth()+
  labs(title = "TPC of Peak Fecundity rate (b)", 
       x = "Temperature (\u00B0C) ", 
       y = "Peak Fecundity rate (1/day)") +
  theme(plot.title = element_text(hjust = 0.5,size = 8, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size= 8),
        axis.title.y=element_text(size= 8))
ggsave("../results/TPC_bpk10.pdf", plot = p_bpk10)


#  k



p10 <- plot_grid(
        p_a10,p_z10,p_zj10,p_bpk10, labels = c("A","B","C","D"), 
        rel_widths = c(1, 1), 
        align = "h", ncol=2,nrow=2)
p10 <- annotate_figure(p10, top = text_grob("10. Aphis gossypii", color = "red", face = "bold", size = 14))
ggsave("../results/TPC_10.pdf", plot = p10)


# 11) "Corythucha ciliata"   

# 12)  "Planococcus citri"      

# 13) "Tetraneura nigri abdominalis"


##### "Lepidoptera" ##################
# 14)"Cydia pomonella"


##### "Hymenoptera" ##################
# 15) "Euplectrus ronnai"      

# 16) "Glyptapanteles muesebecki"  

# 17) "Macrocentrus iridescens"

# 18) "Telenomus chrysopae"  

# 19) "Telenomus isis"    

# 20) "Telenomus lobatus"     

# 21) "Theocolax elegans" 

# 22) "Trichogramma bruni"      

# 23) "Trichogramma sp. nr. Lutea"  

# 24) "Trichogramma sp. nr. Mwanzai"



##### "Psocoptera" ##################
# 25) "Lepinotus reticulatus"



##### "Prostigmata" ##################
# 26) "Tetranychus mcdanieli"


