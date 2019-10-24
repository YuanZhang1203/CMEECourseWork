library(dplyr)
library(lattice)
# load MyDFa
MyDF <- read.csv("../Data/EcolArchives-E089-51-D1.csv",header = T, sep=",", stringsAsFactors = F)

# create variables
MyDF = MyDF %>% 
    mutate(Predator.mass.log = log(Predator.mass),
           Prey.mass.log = log(Prey.mass),
           SizeRatio = Prey.mass / Predator.mass)

# make lattice plots
pdf("../results/Pred_Lattice.pdf", 11.7, 8.3)
densityplot(~log(Predator.mass) | Type.of.feeding.interaction, data=MyDF)
dev.off()

pdf("../results/Prey_Lattice.pdf", 11.7, 8.3)
densityplot(~log(Prey.mass) | Type.of.feeding.interaction, data=MyDF)
dev.off()

pdf("../results/SizeRatio_Lattice.pdf", 11.7, 8.3)
densityplot(~SizeRatio | Type.of.feeding.interaction, data=MyDF)
dev.off()

# create summary csv
pp_results = rbind(
    MyDF %>% group_by(Type.of.feeding.interaction) %>% 
        rename(`Feeding type` = Type.of.feeding.interaction) %>% 
        summarise(
            Variale = "log Predator mass",
            Mean = mean(Predator.mass.log),
            Median = median(Predator.mass.log)),
    MyDF %>% group_by(Type.of.feeding.interaction) %>% 
        rename(`Feeding type` = Type.of.feeding.interaction) %>% 
        summarise(
            Variale = "log Predator mass",
            Mean = mean(Prey.mass.log),
            Median = median(Prey.mass.log)),
    MyDF %>% group_by(Type.of.feeding.interaction) %>% 
        rename(`Feeding type` = Type.of.feeding.interaction) %>% 
        summarise(
            Variale = "Size Ratio",
            Mean = mean(SizeRatio),
            Median = median(SizeRatio))
)
write.csv(pp_results, file = "../results/PP_Results.csv", row.names = F)

