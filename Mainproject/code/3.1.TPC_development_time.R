#!usr/bin/env R
#################################################
# Title: 3(1) TPC_development time in R
# MSc CMEE 
# July 2020 
# Author: YUAN ZHANG 
# refer to: TPC - Development Time.ipynb
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

dev <- subset(data,data$Variable=="Development Time (a)")
dev <- gdata::drop.levels(dev)

levels(dev$originaltraitunit)
levels(dev$interactor1)


##########  Plot ##########

##Aedes albopictus
sp1 <- subset(dev,dev$interactor1=="Aedes albopictus")
sp1 <- gdata::drop.levels(sp1)

levels(sp1$originaltraitunit)

sp1$stdvalue <- 1/sp1$originaltraitvalue
sp1$stdunit <- "Days"
sp1$stdname <- sp1$interactor1

##Aedes camptorhynchus
sp2 <- subset(dev,dev$interactor1=="Aedes camptorhynchus")
sp2 <- gdata::drop.levels(sp2)

levels(sp2$originaltraitunit)

#Has secondary stressor of salinity

sp2$stdvalue <- sp2$originaltraitvalue
sp2$stdunit <- "Days"
sp2$stdname <- sp2$interactor1


##Aedes notoscriptus
sp3 <- subset(dev,dev$interactor1=="Aedes notoscriptus")
sp3 <- gdata::drop.levels(sp3)

levels(sp3$originaltraitunit)

#Has secondary stressor of water type

sp3$stdvalue <- sp3$originaltraitvalue
sp3$stdunit <- "Days"
sp3$stdname <- sp3$interactor1


##Culex annulirostris
sp4 <- subset(dev,dev$interactor1=="Culex annulirostris")
sp4 <- gdata::drop.levels(sp4)

levels(sp4$originaltraitname)
levels(sp4$originaltraitunit)
levels(sp4$originaltraitdef)

sp4$stdvalue <- sp4$originaltraitvalue
sp4$stdunit <- "Days"
sp4$stdname <- paste(sp4$interactor1,"-", sp4$originaltraitdef)

####HAS 1000 VALUE FOR NO DEVELOPMENT#####


##Aphis gossypii
sp5 <- subset(dev,dev$interactor1=="Aphis gossypii")
sp5 <- gdata::drop.levels(sp5)

levels(sp5$originalid)
levels(sp5$originaltraitunit)
levels(sp5$originaltraitname)
levels(sp5$originaltraitdef)

#Development Time from a paper inputted by Cera, didnt have to include development rate since its the same
sp5a <- subset(sp5,sp5$originaltraitunit=="Days")
sp5a$stdvalue <- sp5a$originaltraitvalue
sp5a$stdunit <- "Days"
sp5a$stdname <- sp5a$interactor1


#Development Time from VecTraits
sp5b <- subset(sp5,sp5$originalid=="MTD2084") #Development time, sum of days from Instar I to Instar IV
sp5b$stdvalue <- sp5b$originaltraitvalue
sp5b$stdunit <- "Days"
sp5b$stdname <- paste(sp5b$interactor1, "- Instar I to Instar IV")


sp5c <- subset(sp5,sp5$originalid=="MTD2088") #Generation time, Average interval between the birth of an individual and the birth of its offspring
sp5c$stdvalue <- sp5c$originaltraitvalue
sp5c$stdunit <- "Days"
sp5c$stdname <- paste(sp5b$interactor1, "Time between birth and its first offspring")

sp5 <- rbind(sp5a,sp5b,sp5c)


##Corythucha ciliata
sp6 <- subset(dev,dev$interactor1=="Corythucha ciliata")
sp6 <- gdata::drop.levels(sp6)

levels(sp6$originaltraitname) #Development Rate wont be included, it is inferred from development time
levels(sp6$originaltraitunit) 
levels(sp6$interactor1stage)

sp6 <- subset(sp6, sp6$originaltraitname =="Development Rate")
sp6 <- gdata::drop.levels(sp6)

sp6$stdvalue<- sp6$originaltraitvalue
sp6$stdunit <- "Days"
sp6$stdname <- paste(sp6$interactor1, "-", sp6$interactor1stage)


##Planococcus citri
sp7 <- subset(dev,dev$interactor1=="Planococcus citri")
sp7 <- gdata::drop.levels(sp7)

levels(sp7$originaltraitname)
levels(sp7$originaltraitunit)
levels(sp7$interactor1stage)

sp7a <- subset(sp7,sp7$originaltraitname=="Development Time")
sp7a$stdvalue <- sp7a$originaltraitvalue
sp7a$stdunit <- "Days"
sp7a$stdname <- paste(sp7a$interactor1, "-", sp7a$interactor1stage)

sp7b <- subset(sp7,sp7$originaltraitname=="Generation Time")
sp7b$stdvalue <- sp7b$originaltraitvalue
sp7b$stdunit <- "Days"
sp7b$stdname <- paste(sp7b$interactor1, "Time between birth and its first offspring")

sp7 <- rbind(sp7a,sp7b)


##Tetraneura nigri abdominalis
sp8 <- subset(dev,dev$interactor1=="Tetraneura nigri abdominalis")
sp8 <- gdata::drop.levels(sp8)

levels(sp8$originaltraitunit)
levels(sp8$interactor1stage)

sp8$stdvalue <- sp8$originaltraitvalue
sp8$stdunit <- "Days"
sp8$stdname <- paste(sp8$interactor1, "-", sp8$interactor1stage)


##Euplectrus ronnai

sp9 <- subset(dev,dev$interactor1=="Euplectrus ronnai")
sp9 <- gdata::drop.levels(sp9)

levels(sp9$originaltraitunit)
levels(sp9$interactor1stage)

sp9$stdvalue <- sp9$originaltraitvalue
sp9$stdunit <- "Days"
sp9$stdname <- paste(sp9$interactor1, "-", sp9$interactor1stage)

##Glyptapanteles muesebecki

sp10 <- subset(dev,dev$interactor1=="Glyptapanteles muesebecki")
sp10 <- gdata::drop.levels(sp10)

levels(sp10$originaltraitunit)
levels(sp10$interactor1stage)

sp10$stdvalue <- sp10$originaltraitvalue
sp10$stdunit <- "Days"
sp10$stdname <- paste(sp10$interactor1, "-", sp10$interactor1stage)


##Macrocentrus iridescens

sp11 <- subset(dev,dev$interactor1=="Macrocentrus iridescens")
sp11 <- gdata::drop.levels(sp11)

levels(sp11$originaltraitunit)
levels(sp11$interactor1stage)

sp11$stdvalue <- 1/sp11$originaltraitvalue
sp11$stdunit <- "Days"
sp11$stdname <- paste(sp11$interactor1, "-", sp11$interactor1stage)

##Telenomus isis

sp12 <- subset(dev,dev$interactor1=="Telenomus isis")
sp12 <- gdata::drop.levels(sp12)

levels(sp12$originaltraitunit)
levels(sp12$interactor1stage)
levels(sp12$originaltraitdef)

sp12$stdvalue <- 1/sp12$originaltraitvalue
sp12$stdunit <- "Days"
sp12$stdname <- paste(sp12$interactor1, "-", sp12$interactor1stage)



##Telenomus lobatus

sp13 <- subset(dev,dev$interactor1=="Telenomus lobatus")
sp13 <- gdata::drop.levels(sp13)

levels(sp13$originaltraitunit)
levels(sp13$interactor1stage)
levels(sp13$originaltraitdef)

sp13$stdvalue <- sp13$originaltraitvalue
sp13$stdunit <- "Days"
sp13$stdname <- paste(sp13$interactor1, "-", sp13$interactor1stage)


##Telenomus lobatus

sp13 <- subset(dev,dev$interactor1=="Telenomus lobatus")
sp13 <- gdata::drop.levels(sp13)

levels(sp13$originaltraitunit)
levels(sp13$interactor1stage)
levels(sp13$originaltraitdef)

sp13$stdvalue <- sp13$originaltraitvalue
sp13$stdunit <- "Days"
sp13$stdname <- paste(sp13$interactor1, "-", sp13$interactor1stage)


##Theocolax elegans

sp14 <- subset(dev,dev$interactor1=="Theocolax elegans")
sp14 <- gdata::drop.levels(sp14)

levels(sp14$originaltraitunit)
levels(sp14$interactor1stage)
levels(sp14$originaltraitdef)

sp14$stdvalue <- sp14$originaltraitvalue
sp14$stdunit <- "Days"
sp14$stdname <- paste(sp14$interactor1, "-", sp14$interactor1stage)

##Trichogramma bruni

sp15 <- subset(dev,dev$interactor1=="Trichogramma bruni")
sp15 <- gdata::drop.levels(sp15)

levels(sp15$originaltraitunit)
levels(sp15$interactor1stage)
levels(sp15$originaltraitdef)

sp15$stdvalue <- sp15$originaltraitvalue
sp15$stdunit <- "Days"
sp15$stdname <- paste(sp15$interactor1, "-", sp15$interactor1stage)


##Trichogramma sp. nr. Lutea

sp16 <- subset(dev,dev$interactor1=="Trichogramma sp. nr. Lutea")
sp16 <- gdata::drop.levels(sp16)

levels(sp16$originaltraitunit)
levels(sp16$interactor1stage)
levels(sp16$originaltraitdef)

sp16$stdvalue <- sp16$originaltraitvalue
sp16$stdunit <- "Days"
sp16$stdname <- paste(sp16$interactor1, "-", sp16$interactor1stage)

##Trichogramma sp. nr. Mwanzai

sp17 <- subset(dev,dev$interactor1=="Trichogramma sp. nr. Mwanzai")
sp17 <- gdata::drop.levels(sp17)

levels(sp17$originaltraitunit)
levels(sp17$interactor1stage)
levels(sp17$originaltraitdef)

sp17$stdvalue <- sp17$originaltraitvalue
sp17$stdunit <- "Days"
sp17$stdname <- paste(sp17$interactor1, "-", sp17$interactor1stage)


##Anthonomus grandis

sp18 <- subset(dev,dev$interactor1=="Anthonomus grandis")
sp18 <- gdata::drop.levels(sp18)

levels(sp18$originaltraitunit)
levels(sp18$originaltraitname)

#only using development time, development rate is inferred from development time (and the unit of % is incorrect)

sp18 <- subset(sp18,sp18$originaltraitname=="Development Time")
sp18$stdvalue <- sp18$originaltraitvalue
sp18$stdunit <- "Days"
sp18$stdname <- sp18$interactor1


##Sitona discoideus

sp19 <- subset(dev,dev$interactor1=="Sitona discoideus")
sp19 <- gdata::drop.levels(sp19)

levels(sp19$originaltraitunit)
levels(sp19$interactor1stage)
levels(sp19$originaltraitdef)

sp19$stdvalue <- sp19$originaltraitvalue
sp19$stdunit <- "Days"
sp19$stdname <- paste(sp19$interactor1, "-", sp19$interactor1stage)


##Stethorus punctillum

sp20 <- subset(dev,dev$interactor1=="Stethorus punctillum")
sp20 <- gdata::drop.levels(sp20)

levels(sp20$originaltraitunit)
levels(sp20$interactor1stage)
levels(sp20$originaltraitname)

sp20$stdvalue <- 1/sp20$originaltraitvalue
sp20$stdunit <- "Days"
sp20$stdname <- paste(sp20$interactor1, "-", sp20$interactor1stage)

###A few values are not seen in the graph, all are >300, due to a very very small development rate####

################################# (?)
## Why Alex write Planococcus citri?? BUt the colum names "Planococcus citri"

sp21 <- subset(dev,dev$interactor1=="Planococcus citri") 
sp21 <- gdata::drop.levels(sp21)

levels(sp21$originaltraitunit)
levels(sp21$interactor1stage)
levels(sp21$originaltraitname)

sp21$stdvalue <- sp21$originaltraitvalue
sp21$stdunit <- "Days"
sp21$stdname <- paste(sp21$interactor1, "-", sp21$interactor1stage)


##Cydia pomonella

sp22 <- subset(dev,dev$interactor1=="Cydia pomonella")
sp22 <- gdata::drop.levels(sp22)

levels(sp22$originaltraitunit)
levels(sp22$interactor1stage)
levels(sp22$originaltraitname)

sp22$stdvalue <- sp22$originaltraitvalue
sp22$stdunit <- "Days"
sp22$stdname <- paste(sp22$interactor1, "-", sp22$interactor1stage)


##Tetranychus mcdanieli

sp23 <- subset(dev,dev$interactor1=="Tetranychus mcdanieli")
sp23 <- gdata::drop.levels(sp23)

levels(sp23$originaltraitunit)
levels(sp23$interactor1stage)
levels(sp23$originaltraitname)

sp23$stdvalue <- 1/sp23$originaltraitvalue
sp23$stdunit <- "Days"
sp23$stdname <- paste(sp23$interactor1, "-", sp23$interactor1stage)


##Lepinotus reticulatus

sp24 <- subset(dev,dev$interactor1=="Lepinotus reticulatus")
sp24 <- gdata::drop.levels(sp24)

levels(sp24$originaltraitunit)
levels(sp24$interactor1stage)
levels(sp24$originaltraitname)
levels(sp24$notes)

#Cera somehow wrote the stage in the notes section rather than stage section 
sp24 <- sp24 %>% mutate(interactor1stage= case_when(notes %in% "Combined Immature Stages" ~ 'Combined Immature Stages',
                                                    notes %in% "Combined Nymphal Stages" ~ 'Combined Nymphal Stages',
                                                    notes %in% "Egg" ~ 'Egg',
                                                    notes %in% "" ~ as.character(interactor1stage)))


sp24$interactor1stage

sp24$stdvalue <- sp24$originaltraitvalue
sp24$stdunit <- "Days"
sp24$stdname <- paste(sp24$interactor1, "-", sp24$interactor1stage)


##Telenomus chrysopae: Hymenoptera, missed it above

sp25 <- subset(dev,dev$interactor1=="Telenomus chrysopae")
sp25 <- gdata::drop.levels(sp25)

levels(sp25$originaltraitunit)
levels(sp25$interactor1stage)
levels(sp25$originaltraitdef)

sp25$stdvalue <- sp25$originaltraitvalue
sp25$stdunit <- "Days"
sp25$stdname <- paste(sp25$interactor1, "-", sp25$interactor1stage)

####**** Bactrocera correcta: Diptera, missed in orginal data summary, now added
sp26 <- subset(dev,dev$interactor1=="Bactrocera correcta")
sp26 <- gdata::drop.levels(sp26)

levels(sp26$originaltraitunit)
levels(sp26$interactor1stage)
levels(sp26$originaltraitdef)

sp26a <- subset(sp26, sp26$originaltraitunit=="hour / 1 individual")
sp26a$stdvalue <- sp26a$originaltraitvalue/24
sp26a$stdunit <- "Days"
sp26a$stdname <- paste(sp26a$interactor1, "-", sp26a$interactor1stage)

sp26b <- subset(sp26, sp26$originaltraitunit=="day / 1 individual")
sp26b <- gdata::drop.levels(sp26b)
levels(sp26b$originalid)

#checked original paper, data with ID MTD2165 should be in larval stage rather than pupae
sp26b <- sp26b %>% mutate(interactor1stage= case_when(originalid %in% "MTD2165" ~ 'Larvae',
                                                      originalid %in% "MTD2166" ~ 'Pupae'))

sp26b$stdvalue <- sp26b$originaltraitvalue
sp26b$stdunit <- "Days"
sp26b$stdname <- paste(sp26b$interactor1, "-", sp26b$interactor1stage)

sp26 <- rbind(sp26a,sp26b)




#### check 
ncol(sp1)
ncol(sp2)
ncol(sp3)
ncol(sp4)
ncol(sp5)
ncol(sp6)
ncol(sp7)
ncol(sp8)
ncol(sp9)
ncol(sp10)
ncol(sp11)
ncol(sp12)
ncol(sp13)
ncol(sp14)
ncol(sp15)
ncol(sp16)
ncol(sp17)
ncol(sp18)
ncol(sp19)
ncol(sp20)
ncol(sp21)
ncol(sp22)
ncol(sp23)
ncol(sp24)
ncol(sp25)
ncol(sp26)



## combine the coclumn
stddev <- rbind(sp1,sp2,sp3,sp4,sp5,sp6,sp7,sp8,sp9,sp10,sp11,sp12,sp13,sp14,sp15,sp16,sp17,sp18,sp19,sp20,sp21,sp22,sp23,sp24,sp25,sp26)

str(stddev)

## plot
devTPC <- ggplot(stddev, aes(x=ambienttemp, y=stdvalue)) + geom_point() + theme_bw() + labs(title=expression(paste("Development Time ", (alpha))), x=expression(paste('Temperature (',~degree,'C)',sep='')), y="Development Time (Days)") + theme(plot.title = element_text(hjust = 0.5)) + xlim(0,40) + ylim(0,120) 

plot1 <- devTPC + facet_wrap_paginate(~stdname, ncol=3, nrow=3, page=1, labeller = label_wrap_gen(30)) +  theme(strip.text.x = element_text(size = 10))
plot2 <- devTPC + facet_wrap_paginate(~stdname, ncol=3, nrow=3, page=2, labeller = label_wrap_gen(30)) +  theme(strip.text.x = element_text(size = 10))
plot3 <- devTPC + facet_wrap_paginate(~stdname, ncol=3, nrow=3, page=3, labeller = label_wrap_gen(30)) +  theme(strip.text.x = element_text(size = 10))
plot4 <- devTPC + facet_wrap_paginate(~stdname, ncol=3, nrow=3, page=4, labeller = label_wrap_gen(30)) +  theme(strip.text.x = element_text(size = 10))
plot5 <- devTPC + facet_wrap_paginate(~stdname, ncol=3, nrow=3, page=5, labeller = label_wrap_gen(30)) +  theme(strip.text.x = element_text(size = 10))
plot6 <- devTPC + facet_wrap_paginate(~stdname, ncol=3, nrow=3, page=6, labeller = label_wrap_gen(30)) +  theme(strip.text.x = element_text(size = 10))
plot7 <- devTPC + facet_wrap_paginate(~stdname, ncol=3, nrow=3, page=7, labeller = label_wrap_gen(30)) +  theme(strip.text.x = element_text(size = 10))
plot8 <- devTPC + facet_wrap_paginate(~stdname, ncol=3, nrow=3, page=8, labeller = label_wrap_gen(30)) +  theme(strip.text.x = element_text(size = 10))

ggsave("../results/devTPC1.pdf", plot=plot1)
ggsave("../results/devTPC2.pdf", plot=plot2)
ggsave("../results/devTPC3.pdf", plot=plot3)
ggsave("../results/devTPC4.pdf", plot=plot4)
ggsave("../results/devTPC5.pdf", plot=plot5)
ggsave("../results/devTPC6.pdf", plot=plot6)
ggsave("../results/devTPC7.pdf", plot=plot7)
ggsave("../results/devTPC8.pdf", plot=plot8)


pdf_combine(c("../results/devTPC1.pdf", "../results/devTPC2.pdf","../results/devTPC3.pdf","../results/devTPC4.pdf","../results/devTPC5.pdf","../results/devTPC6.pdf","../results/devTPC7.pdf","../results/devTPC8.pdf"), output = "../results/3.1.TPC-Development Time.pdf")




