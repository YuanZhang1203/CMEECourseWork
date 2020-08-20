#!usr/bin/env R
#################################################
# Title: 3(3) TPC_adult mortality rate in R
# MSc CMEE 
# July 2020 
# Author: YUAN ZHANG 
#################################################

rm(list = ls())
graphics.off()

library(dplyr)
library(ggplot2)
library(gridExtra)
library(ggforce)
library(pdftools)
data <- read.csv("../data/simple.csv")

z <-  subset(data, data$Variable == "Adult Mortality Rate (z)")
z <- gdata::drop.levels(z)


#### check levels
levels(z$stage)

## delete larvae, pupae stages 
z <- z %>% mutate(stagetype = case_when(stage %in% c("", "adult", "Adult", "adult (female)", "adult (male)", "Egg-to-adult") ~ 'adult',
                                        stage %in% c("Juvenile", "larvae", "Larvae", "larvae + pupae", "Egg to L1", "L1 to L2", "L2 to L3", "L3 to L4", "L4 to Pupae","Pupae to Adult" ) ~ 'Juvenile'))
                                           
z <- subset(z,z$stagetype ==  "adult") 
z <- subset(z,z$temp != 0) 
z <- gdata::drop.levels(z)

## check 
levels(z$unit)
levels(z$order)
levels(z$species)


######## classification ############3

# 1. "Anthonomus grandis" , belongs to "Coleoptera"
# (?) did not get the rate from % (slope?)
z1 <- subset(z,z$species == "Anthonomus grandis")
z1 <- gdata::drop.levels(z1)

levels(z1$unit)# confirm unit "%" and  "% of live weevils per day" "days"   

# 1) "%"
z1a <- subset(z1, z1$unit == "%"| z1$unit == "% of live weevils per day")
z1a <- subset(z1a,z1a$time != "NA") 
z1a <- gdata::drop.levels(z1a)

temp_1 <- unique(z1a$temp) # 15 20 25 30 35

# write a loop to plot
for (i in 1:length(temp_1)){
  df1 <- z1a[which(z1a$temp == temp_1[i]),]
  ggplot(df1, aes(x = time, y = traitvalue))+ geom_point() + geom_smooth()+
     labs (title = paste0("Adult mortality rate_Anthonomus grandis: ",temp_1[i], "(\u00B0C)"),#, #'(',~degree,'C)'), 
            x = "Time (days)", 
            y = "Adult mortality proportion (%)")  
  ggsave(paste("../results/", "Adult_mortality_rate_", "Anthonomus grandis:", temp_1[i], ".png", sep = ""), device = png()) # save the respective plots with unique ID 
  dev.off() 
}

## get log version
for (i in 1:length(temp_1)){
  df1 <- z1a[which(z1a$temp == temp_1[i]),]
  df1$logvalue = log(df1$traitvalue)
  ggplot(df1, aes(x = time, y = logvalue))+ geom_point() + geom_smooth()+
    labs (title = paste0("log: Adult mortality rate_Anthonomus grandis: ",temp_1[i], "(\u00B0C)"),#, #'(',~degree,'C)'), 
          x = "Time (days)", 
          y = "log of Adult mortality proportion")  
  ggsave(paste("../results/", "log: Adult_mortality_rate_", "Anthonomus grandis:", temp_1[i], ".png", sep = ""), device = png()) # save the respective plots with unique ID 
  dev.off() 
}

# 2) "days"
z1b <- subset(z1, z1$unit == "days")
z1b  <- gdata::drop.levels(z1b)
z1b$stdvalue <- 1/z1b$traitvalue

zp1 <- ggplot(z1b, aes(x=temp, y = stdvalue)) 
zp1 <- zp1 + geom_point() + geom_smooth()+
       labs(title = "Adult mortality rate: Anthonomus grandis ", 
           x = "Temperature (\u00B0C) ", 
           y = "Adult mortality rate (1/day)")  
ggsave("../results/TPC_z1.pdf", plot=zp1)


# 2 "Stethorus punctillum", belongs to "Coleoptera"
z2 <- subset(z,z$species == "Stethorus punctillum")
z2 <- gdata::drop.levels(z2)

levels(z2$unit)# confirm unit "%" 
# (!) (no time value, just delete)

# levels(z$order)
# "Coleoptera"  "Diptera"     "Hemiptera"   "Hymenoptera" "Prostigmata" "Psocoptera" 
# M <- subset(z, z$order == "Diptera")
# M <- gdata::drop.levels(M)
# levels (M$species)

# 3. "Aedes aegypti", belongs to "Diptera" 
z3 <- subset(z,z$species == "Aedes aegypti")
z3 <- gdata::drop.levels(z3)
levels(z3$unit)# confirm unit "1/day"  "1/days" "days"  

z3a <- subset(z3, z3$unit == "days")
z3a  <- gdata::drop.levels(z3a)
z3a$stdvalue <- 1/z3a$traitvalue

z3b <- subset(z3, z3$unit == "1/day"|z3$unit == "1/days")
z3b  <- gdata::drop.levels(z3b)
z3b$stdvalue <- z3b$traitvalue

z3 <- rbind(z3a, z3b)

zp3 <- ggplot(z3, aes(x=temp, y = stdvalue)) 
zp3 <- zp3 + geom_point() + geom_smooth()+
  labs(title = "Adult mortality rate: Aedes aegypti", 
       x = "Temperature (\u00B0C) ", 
       y = "Adult mortality rate (1/day)")  
ggsave("../results/TPC_z3.pdf", plot=zp3)

# 4.  "Culex annulirostris", belongs to "Diptera" 
z4 <- subset(z,z$species == "Culex annulirostris")
z4 <- gdata::drop.levels(z4)
levels(z4$unit)# confirm unit : "days", "proportion" (!)(delete, because there is no time information) 

z4 <- subset(z4,z4$unit == "days")
z4 <- gdata::drop.levels(z4)
z4$stdvalue <- 1/z4$traitvalue

z4a <- subset(z4,z4$originaltraitname == "Adult longevity (female, bloodfed)")
z4a <- gdata::drop.levels(z4a)

z4b <- subset(z4,z4$originaltraitname == "Adult longevity (male)")
z4b <- gdata::drop.levels(z4b)

zp4a <- ggplot(z4a, aes(x=temp, y = stdvalue)) 
zp4a <- zp4a + geom_point() + geom_smooth()+
  labs(title = "Adult mortality rate: Culex annulirostris (female, bloodfed)", 
       x = "Temperature (\u00B0C) ", 
       y = "Adult mortality rate (1/day)")  
ggsave("../results/TPC_z4a.pdf", plot=zp4a)

zp4b <- ggplot(z4b, aes(x=temp, y = stdvalue)) 
zp4b <- zp4b + geom_point() + geom_smooth()+
  labs(title = "Adult mortality rate: Culex annulirostris (male)", 
       x = "Temperature (\u00B0C) ", 
       y = "Adult mortality rate (1/day)")  
ggsave("../results/TPC_z4b.pdf", plot=zp4b)

# 5 "Aphis gossypii",   belongs to   "Hemiptera"   
z5 <- subset(z,z$species == "Aphis gossypii")
z5 <- gdata::drop.levels(z5)
levels(z5$unit)# confirm unit : "day / 1 individual"; "individuals/day"(!) (I don't know how to handle this situation, many time value within one temperature, so I delete them )

z5 <- subset(z5,z5$originaltraitname == "Longevity")
z5 <- gdata::drop.levels(z5)

z5$stdvalue <- 1/z5$traitvalue
zp5 <- ggplot(z5, aes(x=temp, y = stdvalue)) 
zp5 <- zp5 + geom_point() + geom_smooth()+
  labs(title = "Adult mortality rate: Aphis gossypii (female)", 
       x = "Temperature (\u00B0C) ", 
       y = "Adult mortality rate (1/day)")  
ggsave("../results/TPC_z5.pdf", plot=zp5)

# 6 "Corythucha ciliata"   ,   belongs to   "Hemiptera"  
z6 <- subset(z,z$species == "Corythucha ciliata")
z6 <- gdata::drop.levels(z6)
levels(z6$unit)# confirm unit :  "%" (!)(delete!no time information),   "days"

z6 <- subset(z6,z6$unit == "days")
z6 <- gdata::drop.levels(z6)

z6$stdvalue <- 1/z6$traitvalue
zp6 <- ggplot(z6, aes(x=temp, y = stdvalue)) 
zp6 <- zp6 + geom_point() + geom_smooth()+
  labs(title = "Adult mortality rate: Corythucha ciliata", 
       x = "Temperature (\u00B0C) ", 
       y = "Adult mortality rate (1/day)")  
ggsave("../results/TPC_z6.pdf", plot=zp6)



# 7 "Planococcus citri"  ,   belongs to   "Hemiptera"    
z7 <- subset(z,z$species == "Planococcus citri")
z7 <- gdata::drop.levels(z7)
levels(z7$unit)# confirm unit :  "day / 1 individual"
z7$stdvalue <- 1/z7$traitvalue

z7a <- subset(z7,z7$stage == "adult (female)")
z7a <- gdata::drop.levels(z7a)
z7b <- subset(z7,z7$stage == "adult (male)")
z7b <- gdata::drop.levels(z7b)

zp7a <- ggplot(z7a, aes(x=temp, y = stdvalue)) 
zp7a <- zp7a + geom_point() + geom_smooth()+
  labs(title = "Adult mortality rate: Planococcus citri (female)", 
       x = "Temperature (\u00B0C) ", 
       y = "Adult mortality rate (1/day)")  
ggsave("../results/TPC_z7a.pdf", plot=zp7a)

zp7b <- ggplot(z7b, aes(x=temp, y = stdvalue)) 
zp7b <- zp7b + geom_point() + geom_smooth()+
  labs(title = "Adult mortality rate: Planococcus citri (male)", 
       x = "Temperature (\u00B0C) ", 
       y = "Adult mortality rate (1/day)")  
ggsave("../results/TPC_z7b.pdf", plot=zp7b)

# 8 "Tetraneura nigri abdominalis",   belongs to   "Hemiptera"  
z8 <- subset(z,z$species == "Tetraneura nigri abdominalis")
z8 <- gdata::drop.levels(z8)
levels(z8$unit)# confirm unit :  "day / 1 individual"
z8$stdvalue <- 1/z8$traitvalue

zp8 <- ggplot(z8, aes(x=temp, y = stdvalue)) 
zp8 <- zp8 + geom_point() + geom_smooth()+
  labs(title = "Adult mortality rate: Tetraneura nigri abdominalis (female)", 
       x = "Temperature (\u00B0C) ", 
       y = "Adult mortality rate (1/day)")  
ggsave("../results/TPC_z8.pdf", plot=zp8)


# 9"Telenomus isis" , belongs to   "Hymenoptera"   
z9 <- subset(z,z$species == "Telenomus isis")
z9 <- gdata::drop.levels(z9)
levels(z9$unit)# confirm unit :  "day / 1 individual"
z9$stdvalue <- 1/z9$traitvalue

zp9 <- ggplot(z9, aes(x=temp, y = stdvalue)) 
zp9 <- zp9 + geom_point() + geom_smooth()+
  labs(title = "Adult mortality rate: Telenomus isis (female)", 
       x = "Temperature (\u00B0C) ", 
       y = "Adult mortality rate (1/day)")  
ggsave("../results/TPC_z9.pdf", plot=zp9) ###(?) many groups 

# 10 "Theocolax elegans", belongs to   "Hymenoptera" 
z10 <- subset(z,z$species == "Theocolax elegans")
z10 <- gdata::drop.levels(z10)
levels(z10$unit)# confirm unit :  "day / 1 individual"
z10$stdvalue <- 1/z10$traitvalue

z10a <- subset(z10,z10$stage == "adult (female)")
z10a <- gdata::drop.levels(z10a)
z10b <- subset(z10,z10$stage == "adult (male)")
z10b <- gdata::drop.levels(z10b)

zp10a <- ggplot(z10a, aes(x=temp, y = stdvalue)) 
zp10a <- zp10a + geom_point() + geom_smooth()+
  labs(title = "Adult mortality rate: Theocolax elegans (female)", 
       x = "Temperature (\u00B0C) ", 
       y = "Adult mortality rate (1/day)")  
ggsave("../results/TPC_z10a.pdf", plot=zp10a)

zp10b <- ggplot(z10b, aes(x=temp, y = stdvalue)) 
zp10b <- zp10b + geom_point() + geom_smooth()+
  labs(title = "Adult mortality rate: Theocolax elegans (male)", 
       x = "Temperature (\u00B0C) ", 
       y = "Adult mortality rate (1/day)")  
ggsave("../results/TPC_z10b.pdf", plot=zp10b)


# 11 "Tetranychus mcdanieli", belongs to "Prostigmata"
z11 <- subset(z,z$species == "Tetranychus mcdanieli")
z11 <- gdata::drop.levels(z11)
levels(z11$unit)# confirm unit :  "%" (!) (no time information)


# 12 "Lepinotus reticulatus", belongs to "Psocoptera" 
z12 <- subset(z,z$species == "Lepinotus reticulatus")
z12 <- gdata::drop.levels(z12)
levels(z12$unit)# confirm unit :  "individuals/day"
z12$stdvalue <- z12$traitvalue

zp12 <- ggplot(z12, aes(x=temp, y = stdvalue)) 
zp12 <- zp12 + geom_point() + geom_smooth()+
  labs(title = "Adult mortality rate: Lepinotus reticulatus", 
       x = "Temperature (\u00B0C) ", 
       y = "Adult mortality rate (1/day)")  
ggsave("../results/TPC_z12.pdf", plot=zp12) 


##### combined plot
# z_c <- rbind(z1b, z3,z4a, z4b, z5,z6,z7a,z7b,z8,z9,z10a, z10b,z12)

pdf_combine(c("../results/TPC_z1.pdf","../results/TPC_z3.pdf","../results/TPC_z4a.pdf",
              "../results/TPC_z4b.pdf","../results/TPC_z5.pdf","../results/TPC_z6.pdf",
              "../results/TPC_z7a.pdf","../results/TPC_z7b.pdf","../results/TPC_z8.pdf",
            "../results/TPC_z9.pdf","../results/TPC_z10b.pdf","../results/TPC_z10b.pdf",
            "../results/TPC_z12.pdf"), output = "../results/3.3.TPC_adult mortality rate.pdf")



