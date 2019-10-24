library(dplyr)
library(tidyr)
library(ggplot2)


# load data
dat <- read.csv("../data/EcolArchives-E089-51-D1.csv",header = T, sep=",", stringsAsFactors = T)

# create variables
dat = dat %>% 
    mutate(Predator.mass.log = log(Predator.mass),
           Prey.mass.log = log(Prey.mass),
           SizeRatio = Prey.mass / Predator.mass,
           group = as.factor(paste(Type.of.feeding.interaction, Predator.lifestage)))

# plot
pdf("../Results/PP_Regress_Figure.pdf", 8.3, 11.7)
print(ggplot(data = dat, aes(x=Prey.mass.log, y=Predator.mass.log, color=Predator.lifestage))+
    geom_point(shape=3) + 
    facet_grid(Type.of.feeding.interaction ~ .) +
    geom_smooth(aes(group=group), method='lm', formula = y~x, se=T, fullrange = TRUE) +
    theme(legend.position="bottom") + 
    xlab("log of Prey mass in grams") + ylab("log of Predator mass in grams")
)
dev.off()    

# regression
result = full_join(
    dat %>%
        group_by(Type.of.feeding.interaction, Predator.lifestage) %>%
        group_modify(~ broom::tidy(lm(Predator.mass.log ~ Prey.mass.log, data = .x))) %>% 
        mutate(term = ifelse(term=="Prey.mass.log", "Slope", "Intercept")) %>% 
        ungroup() %>% 
        select(Type.of.feeding.interaction, Predator.lifestage, term, estimate) %>% 
        spread(term, estimate),
    dat %>%
        group_by(Type.of.feeding.interaction, Predator.lifestage) %>%
        group_modify(~ broom::glance(lm(Predator.mass.log ~ Prey.mass.log, data = .x))) %>% 
        ungroup() %>% 
        select(Type.of.feeding.interaction, Predator.lifestage, r.squared, statistic, p.value) %>% 
        rename(`F-statistic` = statistic)
)
write.csv(result, file="../Results/PP_Regress_Results.csv", row.names = F)
