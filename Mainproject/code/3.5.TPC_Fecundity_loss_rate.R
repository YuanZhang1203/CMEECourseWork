#!usr/bin/env R
#################################################
# Title: 3.5.TPC_Fecundity_loss_rate in R
# MSc CMEE 
# July 2020 
# Author: YUAN ZHANG 
#################################################

rm(list = ls())

data <- read.csv("../Data/TraitofInterest.csv")

data <- data %>% mutate(Variable = case_when(originaltraitname %in% c("Development Rate", "Development Time", "Development time","Generation Time","Egg development time") ~ 'Development Time (a)',
                                             originaltraitname %in% c("Fecundity","Fecundity Rate", "Oviposition Rate") ~ 'Peak Fecundity Rate (bpk)',
                                             originaltraitname %in% c("Adult longevity (female, bloodfed)", "Adult longevity (male)","Adult survival","Adult survival (female, bloodfed)", "Adult survival (male)", "Longevity","Mortality Rate","Percentage Survival","Survival Rate","Survivorship", "Survival Time") ~ 'Adult Mortality Rate (z)',
                                             originaltraitname %in% c("Juvenile survival","Juvenile survival ") ~ 'Juvenile Mortality Rate (zJ)',
                                             originaltraitname %in% c("Fecundity","Fecundity Rate", "Oviposition Rate") ~ 'Fecundity Loss Rate (k)'))  #Fecundity loss rate

k <- subset(data,data$Variable=="Adult Mortality Rate (z)")
k <- gdata::drop.levels(k)

levels(k$originaltraitunit)
levels(k$interactor1)


## Aedes aegypti
k1 <- subset(k,k$interactor1=="Aedes aegypti")
k1 <- gdata::drop.levels(k1)

levels(k1$originalid)
levels(k1$originaltraitunit)
levels(k1$originaltraitname)
levels(k1$originaltraitdef)

# 1/day 
k1a <- subset(k1,k1$originaltraitunit==c("1/day", "1/days"))
k1a$stdvalue <- 1/k1a$originaltraitvalue
k1a$stdunit <- "Days"
k1a$stdname <- k1a$interactor1

# others days 
k1b <- subset(k1,k1$originaltraitunit== "days")
k1b$stdvalue <- k1b$originaltraitvalue
k1b$stdunit <- "Days"
k1b$stdname <- k1b$interactor1

k1 <- rbind(k1a,k1b)

## 2 Aedes albopictus albopictus
k2 <- subset(k,k$interactor1=="Aedes albopictus")
k2 <- gdata::drop.levels(k2)

levels(k2$originaltraitunit)
levels(k2$interactor1stage)
levels(k2$originaltraitdef)

# "Egg to L1" 
k2a <- subset(k2,k2$interactor1stage== "Egg to L1" )
k2a <- gdata::drop.levels(k2a)
k2a$stdvalue <- k2a$originaltraitvalue
k2a$stdunit <- "Days"
k2a$stdname <- paste(k2a$interactor1, "-", "Egg to L1")

# "L1 to Adult" 
k2b <- subset(k2,k2$interactor1stage== "L1 to Adult" )
k2b <- gdata::drop.levels(k2b)
k2b$stdvalue <- k2b$originaltraitvalue
k2b$stdunit <- "Days"
k2b$stdname <- paste(k2b$interactor1, "-", "L1 to Adult")

#  "L1 to L2"   
k2c <- subset(k2,k2$interactor1stage==  "L1 to L2" )
k2c <- gdata::drop.levels(k2c)
k2c$stdvalue <- k2c$originaltraitvalue
k2c$stdunit <- "Days"
k2c$stdname <- paste(k2c$interactor1, "-","L1 to L2")

#  "L2 to L3"   
k2d <- subset(k2,k2$interactor1stage==  "L2 to L3"  )
k2d <- gdata::drop.levels(k2d)
k2d$stdvalue <- k2d$originaltraitvalue
k2d$stdunit <- "Days"
k2d$stdname <- paste(k2d$interactor1, "-","L2 to L3" )

#  "L3 to L4"    
k2e <- subset(k2,k2$interactor1stage==  "L3 to L4"    )
k2e <- gdata::drop.levels(k2e)
k2e$stdvalue <- k2e$originaltraitvalue
k2e$stdunit <- "Days"
k2e$stdname <- paste(k2e$interactor1, "-","L3 to L4")

# "L4 to Pupae" 
k2f <- subset(k2,k2$interactor1stage==  "L4 to Pupae"    )
k2f <- gdata::drop.levels(k2f)
k2f$stdvalue <- k2f$originaltraitvalue
k2f$stdunit <- "Days"
k2f$stdname <- paste(k2f$interactor1, "-","L4 to Pupae" )

# "Pupae to Adult"
k2g <- subset(k2,k2$interactor1stage==  "Pupae to Adult"    )
k2g <- gdata::drop.levels(k2g)
k2g$stdvalue <- k2g$originaltraitvalue
k2g$stdunit <- "Days"
k2g$stdname <- paste(k2g$interactor1, "-","Pupae to Adult")


# combine
k2 <- rbind(k2a,k2b,k2c,k2d,k2e,k2f,k2g)


# Anthonomus grandis
k3 <- subset(k,k$interactor1=="Anthonomus grandis")
k3 <- gdata::drop.levels(k3)

levels(k3$originaltraitunit)
levels(k3$interactor1stage)
levels(k3$originaltraitdef)

# "Eggs"
k3a <- subset(k3,k3$interactor1stage=="Eggs")
k3a <- gdata::drop.levels(k3a)
levels(k3a$originaltraitunit)
k3a$stdvalue <- k3a$originaltraitvalue
k3a$stdunit <- "Days"
k3a$stdname <- paste(k3a$interactor1, "-", "Eggs")

# "First Instar"  
k3b <- subset(k3,k3$interactor1stage=="First Instar"  )
k3b <- gdata::drop.levels(k3b)
levels(k3b$originaltraitunit)
k3b$stdvalue <- k3b$originaltraitvalue
k3b$stdunit <- "Days"
k3b$stdname <- paste(k3b$interactor1, "-", "First Instar"  )

# "Larvae"  
k3c <- subset(k3,k3$interactor1stage=="Larvae" )
k3c <- gdata::drop.levels(k3c)
levels(k3c$originaltraitunit)
k3c$stdvalue <- k3c$originaltraitvalue
k3c$stdunit <- "Days"
k3c$stdname <- paste(k3c$interactor1, "-", "Larvae")

#  "Pre-emergent adult"
k3d <- subset(k3,k3$interactor1stage=="Pre-emergent adult" )
k3d <- gdata::drop.levels(k3d)
levels(k3d$originaltraitunit)
k3d$stdvalue <- k3d$originaltraitvalue
k3d$stdunit <- "Days"
k3d$stdname <- paste(k3d$interactor1, "-", "Pre-emergent adult")

#  "Pupae"    
k3e <- subset(k3,k3$interactor1stage== "Pupae" )
k3e <- gdata::drop.levels(k3e)
levels(k3e$originaltraitunit)
k3e$stdvalue <- k3e$originaltraitvalue
k3e$stdunit <- "Days"
k3e$stdname <- paste(k3e$interactor1, "-",  "Pupae")

# "Second Instar" 
k3f <- subset(k3,k3$interactor1stage== "Second Instar" )
k3f <- gdata::drop.levels(k3f)
levels(k3f$originaltraitunit)
k3f$stdvalue <- k3f$originaltraitvalue
k3f$stdunit <- "Days"
k3f$stdname <- paste(k3f$interactor1, "-",  "Second Instar")

# "Third Instar" 
k3g<- subset(k3,k3$interactor1stage== "Third Instar" )
k3g <- gdata::drop.levels(k3g)
levels(k3g$originaltraitunit)
k3g$stdvalue <- k3g$originaltraitvalue
k3g$stdunit <- "Days"
k3g$stdname <- paste(k3g$interactor1, "-",  "Third Instar")

#combine 
k3 <- rbind(k3a,k3b,k3c,k3d,k3e,k3f,k3g)

## 4"Aphis gossypii"   

k4 <- subset(k,k$interactor1=="Aphis gossypii")
k4 <- gdata::drop.levels(k4)

levels(k4$originaltraitunit)
levels(k4$interactor1stage)
levels(k4$originaltraitdef)

# "juvenile"
k4a <- subset(k4,k4$interactor1stage=="juvenile")
k4a <- gdata::drop.levels(k4a)
levels(k4a$originaltraitunit)
k4a$stdvalue <- 1/k4a$originaltraitvalue
k4a$stdunit <- "Days"
k4a$stdname <- paste(k4a$interactor1, "-", "- juvenile")

# "Nymph"
k4b <- subset(k4,k4$interactor1stage=="Nymph")
k4b <- gdata::drop.levels(k4b)
levels(k4b$originaltraitunit)
k4b$stdvalue <- k4b$originaltraitvalue
k4b$stdunit <- "Days"
k4b$stdname <- paste(k4b$interactor1, "-", "- Nymph")

# "adult (female)"
k4c <- subset(k4,k4$interactor1stage=="adult (female)")
k4c <- gdata::drop.levels(k4c)
levels(k4c$originaltraitunit)
k4c$stdvalue <- k4c$originaltraitvalue
k4c$stdunit <- "Days"
k4c$stdname <- paste(k4a$interactor1, "-", "-adult (female)")

# combine
k4 <- rbind(k4a,k4b,k4c)


#"Corythucha ciliata"       

k5 <- subset(k,k$interactor1=="Corythucha ciliata")
k5 <- gdata::drop.levels(k5)

levels(k5$originaltraitunit)

k5$stdvalue <- k5$originaltraitvalue
k5$stdunit <- "Days"
k5$stdname <- k5$interactor1

# 6 "Euplectrus ronnai"  
k6 <- subset(k,k$interactor1=="Euplectrus ronnai")
k6 <- gdata::drop.levels(k6)

levels(k6$originaltraitunit)

k6$stdvalue <- 1/k6$originaltraitvalue
k6$stdunit <- "Days"
k6$stdname <- k6$interactor1


# 7 "Lepinotus reticulatus"   
k7 <- subset(k,k$interactor1=="Lepinotus reticulatus")
k7 <- gdata::drop.levels(k7)

levels(k7$originaltraitunit)

k7$stdvalue <- 1/k7$originaltraitvalue
k7$stdunit <- "Days"
k7$stdname <- k7$interactor1


# 8"Planococcus citri" 
k8 <- subset(k,k$interactor1=="Planococcus citri")
k8 <- gdata::drop.levels(k8)

levels(k8$originaltraitunit)

k8$stdvalue <- k8$originaltraitvalue
k8$stdunit <- "Days"
k8$stdname <- k8$interactor1

# 9"Stethorus punctillum"        
k9 <- subset(k,k$interactor1=="Stethorus punctillum")
k9 <- gdata::drop.levels(k9)

levels(k9$originaltraitunit)

k9$stdvalue <- k9$originaltraitvalue
k9$stdunit <- "Days"
k9$stdname <- k9$interactor1


# 10"Telenomus chrysopae"  
k10 <- subset(k,k$interactor1=="Telenomus chrysopae")
k10 <- gdata::drop.levels(k10)

levels(k10$originaltraitunit)

k10$stdvalue <- 1/k10$originaltraitvalue
k10$stdunit <- "Days"
k10$stdname <- k10$interactor1


# 11"Telenomus isis"     
k11 <- subset(k,k$interactor1=="Telenomus isis")
k11 <- gdata::drop.levels(k11)

levels(k11$originaltraitunit)
levels(k11$interactor1stage)
levels(k4$originaltraitdef)

# "juvenile"
k11a <- subset(k11,k11$interactor1stage=="juvenile")
k11a <- gdata::drop.levels(k11a)
levels(k11a$originaltraitunit)
k11a$stdvalue <- 1/k11a$originaltraitvalue
k11a$stdunit <- "Days"
k11a$stdname <- paste(k11a$interactor1, "-", "- juvenile")

# "adult (female)"
k11b <- subset(k11,k11$interactor1stage=="adult (female)")
k11b <- gdata::drop.levels(k11b)
levels(k11b$originaltraitunit)
k11b$stdvalue <- k11b$originaltraitvalue
k11b$stdunit <- "Days"
k11b$stdname <- paste(k11b$interactor1, "-", "- adult (female)")

# combine
k11 <- rbind(k11a,k11b)

# 12"Telenomus lobatus"       
k12 <- subset(k,k$interactor1=="Telenomus lobatus")
k12 <- gdata::drop.levels(k12)

levels(k12$originaltraitunit)
levels(k12$interactor1stage)
levels(k12$originaltraitdef)

k12$stdvalue <- 1/k12$originaltraitvalue
k12$stdunit <- "Days"
k12$stdname <- paste(k12$interactor1, "-", "- juvenile")

# 13 "Tetraneura nigri abdominalis" 
k13 <- subset(k,k$interactor1=="Tetraneura nigri abdominalis")
k13 <- gdata::drop.levels(k13)

levels(k13$originaltraitunit)
levels(k13$interactor1stage)
levels(k13$originaltraitdef)

k13$stdvalue <- k13$originaltraitvalue
k13$stdunit <- "Days"
k13$stdname <- paste(k13$interactor1, "-", "- adult (female)")


# 14"Tetranychus mcdanieli" 
k14 <- subset(k,k$interactor1=="Tetranychus mcdanieli")
k14 <- gdata::drop.levels(k14)

levels(k14$originaltraitunit)
levels(k14$interactor1stage)
levels(k14$originaltraitdef)

k14$stdvalue <- k14$originaltraitvalue
k14$stdunit <- "Days"
k14$stdname <- k14$interactor1


# 15 "Theocolax elegans" 
k15 <- subset(k,k$interactor1=="Theocolax elegans")
k15 <- gdata::drop.levels(k15)

levels(k15$originaltraitunit)
levels(k15$interactor1stage)
levels(k15$originaltraitdef)

k15$stdvalue <- k15$originaltraitvalue
k15$stdunit <- "Days"
k15$stdname <- k15$interactor1

##combine
k <- rbind(k1, k2,k3,k4,k5,k6,k7,k8,k9,k10,k11,k12,k13,k14,k15)

##plot
kTPC <- ggplot(k, aes(x=ambienttemp, y=stdvalue)) + geom_point() + theme_bw() + labs(title=expression(paste("Fecundity loss rate ", (kappa))), x=expression(paste('Temperature (',~degree,'C)',sep='')), y="Fecundity loss rate") + theme(plot.title = element_text(hjust = 0.5)) + xlim(0,40) + ylim(0,120) 
plot1 <- kTPC + facet_wrap_paginate(~stdname, ncol=3, nrow=3, page=1, labeller = label_wrap_gen(30)) +  theme(strip.text.x = element_text(size = 10))
plot2 <- kTPC + facet_wrap_paginate(~stdname, ncol=3, nrow=3, page=2, labeller = label_wrap_gen(30)) +  theme(strip.text.x = element_text(size = 10))
plot3 <- kTPC + facet_wrap_paginate(~stdname, ncol=3, nrow=3, page=3, labeller = label_wrap_gen(30)) +  theme(strip.text.x = element_text(size = 10))
plot4 <- kTPC + facet_wrap_paginate(~stdname, ncol=3, nrow=3, page=4, labeller = label_wrap_gen(30)) +  theme(strip.text.x = element_text(size = 10))

ggsave("../results/kTPC1.pdf", plot=plot1)
ggsave("../results/kTPC2.pdf", plot=plot2)
ggsave("../results/kTPC3.pdf", plot=plot3)
ggsave("../results/kTPC4.pdf", plot=plot4)

pdf_combine(c("../results/kTPC1.pdf", "../results/kTPC2.pdf","../results/kTPC3.pdf","../results/kTPC4.pdf"), output = "../results/3.5.TPC_Fecundity_loss_rate.pdf")
              
              
              
