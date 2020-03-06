#############################################################################
# Title: resuls plotting 
# CMEE MSc March 2020 
# Author: YUAN ZHANG 
#############################################################################

rm(list = ls())
graphics.off()

#load packages
library("ggplot2")
library("dplyr")

# input data 
s <- read.csv("../Data/statistic_data.csv") 
#### information about this statistical data ####
## Akaike Information Criterion (AIC):
## Bayesian Information Criterion (BIC), also know as the Schwarz Criterion
## For both AIC and BIC, If model A has AIC lower by 2-3 or more than model B, it’s better (The lower the AIC or BIC, the better.)

# get the number of total number of IDs 
Total <- length(unique(s$X))


#### get the percentages of model fitting ####
# get a statistical analytical subset of AIC and BIC data (number of AIC = BIC)
subset <-  rbind(sum(!is.na(s$AIClogistic)), sum(!is.na(s$AICgompertz)), sum(!is.na(s$AICbaranyi)), sum(!is.na(s$AICbuchanan)))

# insert an row to sum the number of data that is not NA
row.names(subset) <- c("logistic","gompertz", "baranyi", "buchanan" )

# get the percentages of model fitting 
subset <- cbind(subset, subset[,1]/Total * 100)

# add row names
colnames(subset) <- c("Fitting_number", "Fitting_percentage")


#### find the smallest AIC/BIC for IDs ####
# get the AIC/BIC subdata
AIC <- data.frame(s$AIClogistic, s$AICgompertz, s$AICbaranyi, s$AICbuchanan)
names(AIC) <- c("logistic","gompertz", "baranyi", "buchanan" )
BIC <- data.frame(s$BIClogistic, s$BICgompertz, s$BICbaranyi, s$BICbuchanan)
names(BIC) <- c("logistic","gompertz", "baranyi", "buchanan" )

# write a loop to get the statistic of smallest number for AIC
for (i in 1: Total){
  a <- AIC[i, ]
  if (sum(is.na(a)) < 4){
    min <- which.min(a) # get the lowest number
    AIC[i, 5] <- as.character(min) # 1 = logistic, 2 = gompertz, 3 = baranyi, 4 = buchanan 
  }else { # if there is no fit model, mark as 0
    AIC[i, 5] <- 0
  }
}

# count the AIC numbber and get the percentage 
 AIC_log <- sum(AIC[,5] == 1) 
 AIC_gom <- sum(AIC[,5] == 2)
 AIC_bar <- sum(AIC[,5] == 3)
 AIC_buc <- sum(AIC[,5] == 4)
 AIC_num <- c(AIC_log, AIC_gom, AIC_bar, AIC_buc)
 subset <- transform(subset, AIC = AIC_num)
 subset <- transform(subset, AIC_percentage = subset[,3]/Total * 100)

 # write a loop to get the statistic of smallest number for BIC
 for (i in 1: Total){
   b <- BIC[i, ]
   if (sum(is.na(b)) < 4){
      min <- which.min(b) # get the lowest number
      BIC[i, 5] <- as.character(min) # 1 = logistic, 2 = gompertz, 3 = baranyi, 4 = buchanan 
   }else { # if there is no fit model, mark as 0
      BIC[i, 5] <- 0
   }
 }
  
 # count the BIC numbber and get the percentage 
 BIC_log <- sum(BIC[,5] == 1) 
 BIC_gom <- sum(BIC[,5] == 2)
 BIC_bar <- sum(BIC[,5] == 3)
 BIC_buc <- sum(BIC[,5] == 4)
 BIC_num <- c(BIC_log, BIC_gom, BIC_bar, BIC_buc)
 subset <- transform(subset, BIC = BIC_num)
 subset <- transform(subset, BIC_percentage = subset[,5]/Total * 100)


subset <- transform(subset, Model = c("Logistic","Gompertz", "Baranyi", "Buchanan"))

# save data of statistic results  
write.csv(subset, file = "../Data/statistic_results.csv") 



#### plot ####
## 1. Model fitting percentage
ggplot(subset, aes(x=Model, y=Fitting_percentage))+
    geom_bar(stat="identity")+
    labs(x = ("Model"), y = paste("Percentage of the model fitting (%)"))+
    geom_text(aes(label= scales::percent(Fitting_percentage/100)), vjust=1.6, color="white", size=3.5)
    
# save this figure 
ggsave(paste("../Results/Fitting_percentage.png", sep = ""), device = png()) 
dev.off() 

## 2. AIC & BIC
# get a new dataset for convinient
count <- c(subset$AIC, subset$BIC)
percentage <- c(subset$AIC_percentage, subset$BIC_percentage)
Methods <- c("AIC", "AIC", "AIC", "AIC", "BIC", "BIC", "BIC", "BIC")
mo <- c("Logistic","Gompertz", "Baranyi", "Buchanan", "Logistic","Gompertz", "Baranyi", "Buchanan")
new_data <- data.frame(count, percentage, Methods, mo)

# get the plot
dev.new()
ggplot(new_data, aes(x=mo, y=percentage, fill = Methods))+
  geom_bar(position="dodge", stat="identity")+
  labs(x = ("Model"), y = paste("Percentage of the best fitting model(%)"))+
  geom_text(stat="identity",aes(label = scales::percent(percentage/100)), color="black", size=3.5,position=position_dodge(0.8),vjust=-0.8)

ggsave(paste("../Results/Bestfitting.png", sep = ""), device = png()) 
dev.off()








