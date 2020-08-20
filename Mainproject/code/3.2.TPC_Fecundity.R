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

#--------- Load some packages --------#
library("dplyr")
library("ggplot2")
library("gridExtra")
library("ggforce")
library("pdftools")
library("ggpubr")

#--------- Load dataset and check levels --------#
data <- read.csv("../data/simple.csv")

b <- subset(data, data$Variable == "Fecundity")
b <- gdata::drop.levels(b)

unique((b$species))
#-----------------------------------------------------------------------------------------------------------------#

#-----------------------------------------------------------------------------------------------------------------#

# "Tetraneura nigri abdominalis" 
# "Aedes camptorhynchus"      
# "Culex annulirostris"         
# "Aedes aegypti"               
# "Aphis gossypii"               
# "Corythucha ciliata"          
# "Aedes albopictus"             


## "Coleoptera" ## 
# 1. only one species : "Anthonomus grandis"
b1 <- subset(b, b$species == "Anthonomus grandis")
b1 <- gdata::drop.levels(b1)
b1$stdvalue <- b1$traitvalue
b1$logvalue <- log(b1$stdvalue)

# check units
unique(b1$unit) # "Eggs/female/day"

# plot b ~ t
bpk1 <- c()
k1 <- c()
temp1 <- unique(b1$temp)  # get temperature ranges: 15 20 25 30 35

for (i in 1: length(temp1)) {
  df <- b1[which(b1$temp == temp1[i]),]
  bpk1[i] = max(df$traitvalue)
  
  df$stdvalue <- df$traitvalue
  pbt <- ggplot(df, aes(x = time, y = stdvalue))+ geom_point() + geom_smooth()+
          labs(title = paste("Anthonomus grandis",temp1[i],"(\u00B0C)",sep = "" ),
          x = "Time (days)", 
          y = "Fecundity (Eggs/female/day)")+ 
          theme(plot.title = element_text(hjust = 0.5,size = 10, face = "bold.italic"),
             axis.text=element_text(size=8,face = "bold"),
             axis.title.x=element_text(size= 10),
             axis.title.y=element_text(size= 10))
  ggsave(paste("../Results/bt_1.Anthonomus grandis:", temp1[i], ":.png",sep = ""), device = png())
  lm <- lm(logvalue ~ time, data = df)
  k1[1] <- coef(lm)[2]
}
bpk1 <- data.frame(bpk1, temp1, k1)

pbpk1< ggplot(bpk1, aes(x = temp1, y = bpk1))+ geom_point() + geom_smooth()+
  labs(title = paste("Anthonomus grandis",te,"(\u00B0C)",sep = "" ),
       x = "Time (days)", 
       y = "Fecundity (Eggs/female/day)")+ 
  theme(plot.title = element_text(hjust = 0.5,size = 10, face = "bold.italic"),
        axis.text=element_text(size=8,face = "bold"),
        axis.title.x=element_text(size= 10),
        axis.title.y=element_text(size= 10))
ggsave(paste("../Results/bt_1.Anthonomus grandis:", temp1[i], ":.png",sep = ""), device = png())


# A15 
A15 <- subset(Ant, Ant$ambienttemp == "15")
A15 <- gdata::drop.levels(A15)
A15$time <-A15$timestart
A15$stdunit <- "Days"
A15$stdvalue <- A15$originaltraitvalue
A15$stdname  <- paste(unique(A15$ambienttemp),"celcius")
A15$logvalue <- log(A15$stdvalue)
  
p15 <- ggplot(A15, aes(x = time, y = stdvalue))
A1 <- p15 + geom_point() + geom_smooth()+
     labs(title=expression(paste('Anthonomus grandis 15 (',~degree,'C)',sep='')), 
     x = "Time (days)", 
     y = "Fecundity (Eggs/female/day)")

A15.lm <- lm(log(stdvalue) ~ time, data = A15)

test <- ggplot(A15, aes(x = time, y = logvalue))
test <- test + geom_point() + geom_smooth()+
  labs(title=expression(paste('Anthonomus grandis 15 (',~degree,'C)',sep='')), 
       x = "Time (days)", 
       y = "Log of Fecundity (Eggs/female/day)")

A15_k <- coef(A15.lm)[2]

# A20 
A20 <- subset(Ant, Ant$ambienttemp == "20")
A20 <- gdata::drop.levels(A20)
A20$time <-A20$timestart
A20$stdunit <- "Days"
A20$stdvalue <- A20$originaltraitvalue
A20$stdname  <- paste(unique(A20$ambienttemp),"celcius")

p20 <- ggplot(A20, aes(x = time, y = stdvalue))
A2 <- p20 + geom_point() + geom_smooth()+
  labs(title=expression(paste('Anthonomus grandis 20 (',~degree,'C)',sep='')), 
       x = "Time (days)", 
       y = "Fecundity (Eggs/female/day)")

A20.lm <- lm(stdvalue ~ time, data = A20)
A20_k <- coef(A20.lm)[2]

# A25 
A25 <- subset(Ant, Ant$ambienttemp == "25")
A25 <- gdata::drop.levels(A25)
A25$time <-A25$timestart
A25$stdunit <- "Days"
A25$stdvalue <- A25$originaltraitvalue
A25$stdname <- paste(unique(A25$ambienttemp),"celcius")

p25 <- ggplot(A25, aes(x = time, y = stdvalue))
A3 <- p25 + geom_point() + geom_smooth()+
  labs(title=expression(paste('Anthonomus grandis 25 (',~degree,'C)',sep='')), 
       x = "Time (days)", 
       y = "Fecundity (Eggs/female/day)")

A25.lm <- lm(stdvalue ~ time, data = A25)
A25_k <- coef(A25.lm)[2]

# A30 
A30 <- subset(Ant, Ant$ambienttemp == "30")
A30 <- gdata::drop.levels(A30)
A30$time <-A30$timestart
A30$stdunit <- "Days"
A30$stdvalue <- A30$originaltraitvalue
A30$stdname <- paste(unique(A30$ambienttemp),"celcius")

p30 <- ggplot(A30, aes(x = time, y = stdvalue))
A4 <- p30 + geom_point() + geom_smooth()+
  labs(title=expression(paste('Anthonomus grandis 30 (',~degree,'C)',sep='')), 
       x = "Time (days)", 
       y = "Fecundity (Eggs/female/day)")

A30.lm <- lm(stdvalue ~ time, data = A30)
A30_k <- coef(A30.lm)[2]

# A35
A35 <- subset(Ant, Ant$ambienttemp == "35")
A35 <- gdata::drop.levels(A35)
A35$time <-A35$timestart
A35$stdunit <- "Days"
A35$stdvalue <- A35$originaltraitvalue
A35$stdname <- paste(unique(A35$ambienttemp),"celcius")

p35 <- ggplot(A35, aes(x = time, y = stdvalue))
A5 <- p35 + geom_point() + geom_smooth()+
  labs(title=expression(paste('Anthonomus grandis 35 (',~degree,'C)',sep='')), 
       x = "Time (days)", 
       y = "Fecundity (Eggs/female/day)")

A35.lm <- lm(stdvalue ~ time, data = A35)
A35_k <- coef(A35.lm)[2]

# combine 
time_b_t <- rbind(A15, A20, A25, A30, A35)
b_t <- ggplot(time_b_t, aes(x=time, y=stdvalue)) + geom_point() + theme_bw() + 
  labs(title=expression(paste("Anthonomus grandis_","Fecundity", (b), "~Time")), 
                        x = "Time (days)", 
                        y = "Fecundity (Eggs/female/day)")
       
plot1 <- b_t + facet_wrap(~ stdname, ncol = 2, scales = "free")+ geom_smooth() 

# plot bpk and k ~ temperature
Abpk <- c(max(A15$stdvalue), max(A20$stdvalue), max(A25$stdvalue), max(A30$stdvalue), max(A35$stdvalue)) 
Ak <- c(A15_k, A20_k, A25_k, A30_k, A35_k)
At <- c(15,20,25,30,35)
data_b_A <- data.frame(Abpk,Ak, At)

plot_bpk1 <- ggplot(data_b_A, aes(x = At, y = Abpk)) + geom_point()+
       labs(title="Peak Fecundity Rate (bpk)_Anthonomus grandis", 
       x = expression(paste('Temperature (',~degree,'C)',sep='')), 
       y = "Peak Fecundity Rate (Eggs/female/day)")

plot_k1 <- ggplot(data_b_A, aes(x = At, y = Ak)) + geom_point()+
       labs(title="Fecundity Loss Rate (k)_Anthonomus grandis", 
       x = expression(paste('Temperature (',~degree,'C)',sep='')), 
       y = "Fecundity Loss Rate (k)") 
  
# save 
ggsave("../results/Anthonomus_grandis_b_time15.pdf", plot=A1)
ggsave("../results/Anthonomus_grandis_b_time20.pdf", plot=A2)
ggsave("../results/Anthonomus_grandis_b_time25.pdf", plot=A3)
ggsave("../results/Anthonomus_grandis_b_time30.pdf", plot=A4)
ggsave("../results/Anthonomus_grandis_b_time35.pdf", plot=A5)

ggsave("../results/b_time1.pdf", plot=plot1)

ggsave("../results/bpk_tem1.pdf", plot=plot_bpk1)
ggsave("../results/k_tem1.pdf", plot=plot_k1)

## 2. "Diptera"    
Dip <- subset(fecund, fecund$interactor1order =="Diptera")
Dip <- gdata::drop.levels(Dip)
levels(Dip$interactor1)

# 1) "Aedes aegypti"  
# get subset of species
aegypti <- subset(Dip, Dip$interactor1 =="Aedes aegypti")
aegypti <- gdata::drop.levels(aegypti)

# check units
levels(aegypti$originaltraitunit) # unit: per day per female
# (?) no time information, so no peak value and k

# 2) "Aedes albopictus"  
# get subset of species
albopictus <- subset(Dip, Dip$interactor1 =="Aedes albopictus")
albopictus <- gdata::drop.levels(albopictus)

# check units
levels(albopictus$originaltraitunit) # unit:"eggs per female per cycle"
# (?) no time information, so no peak value and k

# 3) "Aedes camptorhynchus"
# get subset of species
camptorhynchus <- subset(Dip, Dip$interactor1 =="Aedes camptorhynchus")
camptorhynchus <- gdata::drop.levels(camptorhynchus)

# check units
levels(camptorhynchus$originaltraitunit) # unit: eggs
# (?) no time information, so no peak value and k

# 4) "Culex annulirostris" 
# get subset of species
annulirostris <- subset(Dip, Dip$interactor1 =="Culex annulirostris")
annulirostris <- gdata::drop.levels(annulirostris)

# check units
levels(Cole$originaltraitunit)
# (?) no time information, so no peak value and k

## 3. "Hemiptera" 
Hemi <- subset(fecund, fecund$interactor1order =="Hemiptera")
Hemi <- gdata::drop.levels(Hemi)
levels(Hemi$interactor1)

# 1) "Aphis gossypii"  
# get subset of species
gossypii <- subset(Hemi, Hemi$interactor1 =="Aphis gossypii")
gossypii <- gdata::drop.levels(gossypii)

# check units
levels(gossypii$originaltraitunit) # unit: "nymphs/female/day"

# check the temperature range 
unique(gossypii$ambienttemp)

# 15.0 
Go15.0 <- subset(gossypii, gossypii$ambienttemp == 15.0)
Go15.0 <- gdata::drop.levels(Go15.0)
Go15.0$time <- Go15.0$timestart
Go15.0$stdunit <- "Days"
Go15.0$stdvalue <- Go15.0$originaltraitvalue
Go15.0$stdname  <- paste(unique(Go15.0$ambienttemp),"celcius")

Go_p15.0 <- ggplot(Go15.0, aes(x = time, y = stdvalue))
Go_p15.0 <- Go_p15.0 + geom_point() + geom_smooth()+
  labs(title=expression(paste('Aphis gossypii 15.0 (',~degree,'C)',sep='')), 
       x = "Time (days)", 
       y = "Fecundity (nymphs/female/day)")

bpk_Go15.0  <- max(Go15$stdvalue)
Go15.0sub <- subset(Go15.0, Go15.0$stdvalue == max(Go15.0$stdvalue)) 
Go15.0sub$timestart

Go15.0sub2 <- subset(Go15.0, Go15.0$timestart >= 21) 

Go15.0.lm <- lm(stdvalue ~ time, data = Go15.0sub2)
k_Go15.0 <- coef(Go15.0.lm)[2]

# 17.5 
Go17.5 <- subset(gossypii, gossypii$ambienttemp == 17.5)
Go17.5 <- gdata::drop.levels(Go17.5)
Go17.5$time <- Go17.5$timestart
Go17.5$stdunit <- "Days"
Go17.5$stdvalue <- Go17.5$originaltraitvalue
Go17.5$stdname  <- paste(unique(Go17.5$ambienttemp),"celcius")

Go_p17.5 <- ggplot(Go17.5, aes(x = time, y = stdvalue))
Go_p17.5  <- Go_p17.5 + geom_point() + geom_smooth()+
  labs(title=expression(paste('Aphis gossypii 17.5 (',~degree,'C)',sep='')), 
       x = "Time (days)", 
       y = "Fecundity (nymphs/female/day)")

bpk_Go17.5 <- max(Go17.5$stdvalue)
Go17.5sub <- subset(Go17.5, Go17.5$stdvalue == max(Go17.5$stdvalue)) 
Go17.5sub$timestart

Go17.5sub2 <- subset(Go17.5, Go17.5$timestart >= Go17.5sub$timestart) 

Go17.5.lm <- lm(stdvalue ~ time, data = Go17.5sub2)
k_Go17.5 <- coef(Go17.5.lm)[2]

# 20.0 
Go20.0 <- subset(gossypii, gossypii$ambienttemp == 20.0)
Go20.0 <- gdata::drop.levels(Go20.0)
Go20.0$time <- Go20.0$timestart
Go20.0$stdunit <- "Days"
Go20.0$stdvalue <- Go20.0$originaltraitvalue
Go20.0$stdname  <- paste(unique(Go20.0$ambienttemp),"celcius")

Go_p20.0 <- ggplot(Go20.0, aes(x = time, y = stdvalue))
Go_p20.0  <- Go_p20.0 + geom_point() + geom_smooth()+
  labs(title=expression(paste('Aphis gossypii 20.0 (',~degree,'C)',sep='')), 
       x = "Time (days)", 
       y = "Fecundity (nymphs/female/day)")

bpk_Go20.0 <- max(Go20.0$stdvalue)
Go20.0sub <- subset(Go20.0, Go20.0$stdvalue == max(Go20.0$stdvalue)) 
Go20.0sub$timestart

Go20.0sub2 <- subset(Go20.0, Go20.0$timestart >= Go20.0sub$timestart) 

Go20.0.lm <- lm(stdvalue ~ time, data = Go20.0sub2)
k_Go20.0 <- coef(Go20.0.lm)[2]

# 22.5 
Go22.5 <- subset(gossypii, gossypii$ambienttemp == 22.5)
Go22.5 <- gdata::drop.levels(Go22.5)
Go22.5$time <- Go22.5$timestart
Go22.5$stdunit <- "Days"
Go22.5$stdvalue <- Go22.5$originaltraitvalue
Go22.5$stdname  <- paste(unique(Go22.5$ambienttemp),"celcius")

Go_p22.5 <- ggplot(Go22.5, aes(x = time, y = stdvalue))
Go_p22.5  <- Go_p22.5 + geom_point() + geom_smooth()+
  labs(title=expression(paste('Aphis gossypii 22.5 (',~degree,'C)',sep='')), 
       x = "Time (days)", 
       y = "Fecundity (nymphs/female/day)")

bpk_Go22.5<- max(Go22.5$stdvalue)
Go22.5sub <- subset(Go22.5, Go22.5$stdvalue == max(Go22.5$stdvalue)) 
Go22.5sub$timestart

Go22.5sub2 <- subset(Go22.5, Go22.5$timestart >= Go22.5sub$timestart) 

Go22.5.lm <- lm(stdvalue ~ time, data = Go22.5sub2)
k_Go22.5 <- coef(Go22.5.lm)[2]

# 25.0 
Go25.0 <- subset(gossypii, gossypii$ambienttemp == 25.0)
Go25.0 <- gdata::drop.levels(Go25.0)
Go25.0$time <- Go25.0$timestart
Go25.0$stdunit <- "Days"
Go25.0$stdvalue <- Go25.0$originaltraitvalue
Go25.0$stdname  <- paste(unique(Go25.0$ambienttemp),"celcius")

Go_p25.0 <- ggplot(Go25.0, aes(x = time, y = stdvalue))
Go_p25.0  <- Go_p25.0 + geom_point() + geom_smooth()+
  labs(title=expression(paste('Aphis gossypii 25.0 (',~degree,'C)',sep='')), 
       x = "Time (days)", 
       y = "Fecundity (nymphs/female/day)")

bpk_Go25.0 <- max(Go25.0$stdvalue)
Go25.0sub <- subset(Go25.0, Go25.0$stdvalue == max(Go25.0$stdvalue)) 
Go25.0sub$timestart

Go25.0sub2 <- subset(Go25.0, Go25.0$timestart >= Go25.0sub$timestart) 

Go25.0.lm <- lm(stdvalue ~ time, data = Go25.0sub2)
k_Go25.0 <- coef(Go25.0.lm)[2]

# 27.5 
Go27.5 <- subset(gossypii, gossypii$ambienttemp == 27.5)
Go27.5 <- gdata::drop.levels(Go27.5)
Go27.5$time <- Go27.5$timestart
Go27.5$stdunit <- "Days"
Go27.5$stdvalue <- Go27.5$originaltraitvalue
Go27.5$stdname  <- paste(unique(Go27.5$ambienttemp),"celcius")

Go_p27.5 <- ggplot(Go27.5, aes(x = time, y = stdvalue))
Go_p27.5  <- Go_p27.5 + geom_point() + geom_smooth()+
  labs(title=expression(paste('Aphis gossypii 27.5 (',~degree,'C)',sep='')), 
       x = "Time (days)", 
       y = "Fecundity (nymphs/female/day)")

bpk_Go27.5<- max(Go27.5$stdvalue)
Go27.5sub <- subset(Go27.5, Go27.5$stdvalue == max(Go27.5$stdvalue)) 
Go27.5sub$timestart

Go27.5sub2 <- subset(Go27.5, Go27.5$timestart >= Go27.5sub$timestart) 

Go27.5.lm <- lm(stdvalue ~ time, data = Go27.5sub2)
k_Go27.5 <- coef(Go27.5.lm)[2]
k_Go27.5

# 30.0 
Go30.0 <- subset(gossypii, gossypii$ambienttemp == 30.0)
Go30.0 <- gdata::drop.levels(Go30.0)
Go30.0$time <- Go30.0$timestart
Go30.0$stdunit <- "Days"
Go30.0$stdvalue <- Go30.0$originaltraitvalue
Go30.0$stdname  <- paste(unique(Go30.0$ambienttemp),"celcius")

Go_p30.0 <- ggplot(Go30.0, aes(x = time, y = stdvalue))
Go_p30.0  <- Go_p30.0 + geom_point() + geom_smooth()+
  labs(title=expression(paste('Aphis gossypii 30.0 (',~degree,'C)',sep='')), 
       x = "Time (days)", 
       y = "Fecundity (nymphs/female/day)")

bpk_Go30.0 <- max(Go30.0$stdvalue)
Go30.0sub <- subset(Go30.0, Go30.0$stdvalue == max(Go30.0$stdvalue)) 
Go30.0sub$timestart

Go30.0sub2 <- subset(Go30.0, Go30.0$timestart >= Go30.0sub$timestart) 

Go30.0.lm <- lm(stdvalue ~ time, data = Go30.0sub2)
k_Go30.0  <- coef(Go30.0.lm)[2]
k_Go30.0

# 32.5
Go32.5 <- subset(gossypii, gossypii$ambienttemp == 32.5)
Go32.5 <- gdata::drop.levels(Go32.5)
Go32.5$time <- Go32.5$timestart
Go32.5$stdunit <- "Days"
Go32.5$stdvalue <- Go32.5$originaltraitvalue
Go32.5$stdname  <- paste(unique(Go32.5$ambienttemp),"celcius")

Go_p32.5 <- ggplot(Go32.5, aes(x = time, y = stdvalue))
Go_p32.5  <- Go_p32.5 + geom_point() + geom_smooth()+
  labs(title=expression(paste('Aphis gossypii 32.5 (',~degree,'C)',sep='')), 
       x = "Time (days)", 
       y = "Fecundity (nymphs/female/day)")

bpk_Go32.5 <- max(Go32.5$stdvalue)
Go32.5sub <- subset(Go32.5, Go32.5$stdvalue == max(Go32.5$stdvalue)) 
Go32.5sub$timestart

Go32.5sub2 <- subset(Go32.5, Go32.5$timestart >= Go32.5sub$timestart) 

Go32.5.lm <- lm(stdvalue ~ time, data = Go32.5sub2)
k_Go32.5 <- coef(Go32.5.lm)[2]
k_Go32.5

# combine and plot
bpk_Go <- c(bpk_Go15.0, bpk_Go17.5, bpk_Go20.0, bpk_Go22.5, bpk_Go25.0, bpk_Go27.5, bpk_Go30.0, bpk_Go32.5) 
k_Go <- c(k_Go15.0, k_Go17.5, k_Go20.0, k_Go22.5, k_Go25.0, k_Go27.5, k_Go30.0, k_Go32.5)
Temp_Go <- c(15.0, 17.5, 20.0, 22.5, 25.0, 27.5, 30.0, 32.5)
data_b_Go <- data.frame(bpk_Go, k_Go, Temp_Go)

plot_bpk2 <- ggplot(data_b_Go, aes(x = Temp_Go, y = bpk_Go)) + geom_point()+
  labs(title="Peak Fecundity Rate (bpk)_Aphis gossypii", 
       x = expression(paste('Temperature (',~degree,'C)',sep='')), 
       y = "Peak Fecundity Rate (nymphs/female/day)")

plot_k2 <- ggplot(data_b_Go, aes(x = Temp_Go, y = k_Go)) + geom_point()+
  labs(title="Fecundity Loss Rate (k)_Aphis gossypii", 
       x = expression(paste('Temperature (',~degree,'C)',sep='')), 
       y = "Fecundity Loss Rate (k)") 

time_b_t2 <- rbind(Go15.0, Go17.5, Go20.0, Go22.5, Go25.0, Go27.5, Go30.0, Go32.5) 
b_t2 <- ggplot(time_b_t2, aes(x=time, y=stdvalue)) + geom_point() + theme_bw() + 
  labs(title=expression(paste("Aphis gossypii_","Fecundity", (b), "~Time")), 
       x = "Time (days)", 
       y = "Fecundity")

plot2 <- b_t2 + facet_wrap(~ stdname, ncol = 2, scales = "free")+ geom_smooth() 
plot2

# save 
ggsave("../results/Aphis gossypii_b_time15.0.pdf", plot=Go_p15.0)
ggsave("../results/Aphis gossypii_b_time17.5.pdf", plot=Go_p17.5)
ggsave("../results/Aphis gossypii_b_time20.0.pdf", plot=Go_p20.0)
ggsave("../results/Aphis gossypii_b_time22.5.pdf", plot=Go_p22.5)
ggsave("../results/Aphis gossypii_b_time25.0.pdf", plot=Go_p25.0)
ggsave("../results/Aphis gossypii_b_time27.5.pdf", plot=Go_p27.5)
ggsave("../results/Aphis gossypii_b_time30.0.pdf", plot=Go_p30.0)
ggsave("../results/Aphis gossypii_b_time32.5.pdf", plot=Go_p32.5)

ggsave("../results/b_time2.pdf", plot=plot2)
ggsave("../results/bpk_tem2.pdf", plot=plot_bpk2)
ggsave("../results/k_tem2.pdf", plot=plot_k2)

# 2) "Corythucha ciliata"    
# get subset of species
ciliata <- subset(Hemi, Hemi$interactor1 =="Corythucha ciliata")
ciliata <- gdata::drop.levels(ciliata)

# check units
levels(ciliata$originaltraitunit) # unit:eggs per female
unique(ciliata$ambienttemp)

# check time
# (?) no time information, so no peak value and k

# 3) "Tetraneura nigri abdominalis"
# get subset of species
nig <- subset(Hemi, Hemi$interactor1 =="Tetraneura nigri abdominalis")
nig <- gdata::drop.levels(nig)

# check units
levels(nig$originaltraitunit)
unique(nig$ambienttemp)

# check time
# (?) no time information, so no peak value and k



########### optput ############
# 3.2.1.b ~ time
pdf_combine(c("../results/b_time1.pdf","../results/b_time2.pdf" ), output = "../results/3.2.1.Fecundity_time.pdf")

# 3.2.2.bpk ~ temperature
pdf_combine(c("../results/bpk_tem1.pdf","../results/bpk_tem2.pdf" ), output = "../results/3.2.2.Peak_Fecundity_Rate_temperature.pdf")

# 3.2.2. k ~ temperature
pdf_combine(c("../results/k_tem1.pdf","../results/k_tem2.pdf" ), output = "../results/3.2.3.Fecundity_Loss_Rate_temperature.pdf")









