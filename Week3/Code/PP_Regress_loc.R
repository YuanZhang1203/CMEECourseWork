library(dplyr)
library(tidyr)


# load data
dat <- read.csv("../data/EcolArchives-E089-51-D1.csv",header = T, sep=",", stringsAsFactors = T)

# create variables
dat = dat %>% 
    mutate(Predator.mass.log = log(Predator.mass),
           Prey.mass.log = log(Prey.mass),
           SizeRatio = Prey.mass / Predator.mass,
           group = as.factor(paste(Type.of.feeding.interaction, Predator.lifestage)))

# regression
result = full_join(
    dat %>%
        group_by(Type.of.feeding.interaction, Predator.lifestage, Location) %>%
        group_modify(~ broom::tidy(lm(Predator.mass.log ~ Prey.mass.log, data = .x))) %>% 
        mutate(term = ifelse(term=="Prey.mass.log", "Slope", "Intercept")) %>% 
        ungroup() %>% 
        select(Type.of.feeding.interaction, Predator.lifestage, Location, term, estimate) %>% 
        spread(term, estimate),
    dat %>%
        group_by(Type.of.feeding.interaction, Predator.lifestage, Location) %>%
        group_modify(~ broom::glance(lm(Predator.mass.log ~ Prey.mass.log, data = .x))) %>% 
        ungroup() %>% 
        select(Type.of.feeding.interaction, Predator.lifestage, Location, r.squared, statistic, p.value) %>% 
        rename(`F-statistic` = statistic)
)

write.csv(result, file="../Results/PP_Regress_loc_Results.csv", row.names = F)
