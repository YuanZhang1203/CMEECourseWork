#!usr/bin/env R
#################################################
# Title: 2.2. data available plot in R
# MSc CMEE 
# July 2020 
# Author: YUAN ZHANG 
#################################################

rm(list = ls())
graphics.off()

library("dplyr")
library("gridExtra")

summary <- read.csv("../Data/summary.csv")


## plot table
levels(summary$Order)
# 1 "diptera" 
diptera <- subset(summary, summary$Order == "Diptera")
diptera <- gdata::drop.levels(diptera)
levels(diptera$Species)

# 1) Aedes aegypti
Aedes_aegypti <- subset(diptera, diptera$Species == "Aedes aegypti")
Aedes_aegypti <- gdata::drop.levels(Aedes_aegypti)

# 2) Aedes albopictus
Aedes_albopictus <- subset(diptera, diptera$Species == "Aedes albopictus")
Aedes_albopictus <- gdata::drop.levels(Aedes_albopictus)

# 3) "Aedes camptorhynchus"
Aedes_camptorhynchus <- subset(diptera, diptera$Species == "Aedes camptorhynchus")
Aedes_camptorhynchus <- gdata::drop.levels(Aedes_camptorhynchus)

# 4) "Aedes notoscriptus"  
Aedes_notoscriptus <- subset(diptera, diptera$Species == "Aedes notoscriptus")
Aedes_notoscriptus <- gdata::drop.levels(Aedes_notoscriptus)

# 5) "Bactrocera correcta"  
Bactrocera_correcta <- subset(diptera, diptera$Species == "Bactrocera correcta")
Bactrocera_correcta <- gdata::drop.levels(Bactrocera_correcta)

# 6) "Culex annulirostris" 
Culex_annulirostris <- subset(diptera, diptera$Species == "Culex annulirostris")
Culex_annulirostris <- gdata::drop.levels(Culex_annulirostris)

# 2 "Coleoptera" 
Coleoptera <- subset(summary, summary$Order == "Coleoptera")
Coleoptera <- gdata::drop.levels(Coleoptera)
levels(Coleoptera$Species)

# 1) "Anthonomus grandis" 
Anthonomus_grandis <- subset(Coleoptera, Coleoptera$Species == "Anthonomus grandis")
Anthonomus_grandis <- gdata::drop.levels(Anthonomus_grandis)

# 2) "Sitona discoideus"
Sitona_discoideus <- subset(Coleoptera, Coleoptera$Species == "Sitona discoideus")
Sitona_discoideus <- gdata::drop.levels(Sitona_discoideus)

# 3) "Stethorus punctillum"
Stethorus_punctillum <- subset(Coleoptera, Coleoptera$Species == "Stethorus punctillum")
Stethorus_punctillum <- gdata::drop.levels(Stethorus_punctillum)

# 3 "Hemiptera"  
Hemiptera <- subset(summary, summary$Order == "Hemiptera")
Hemiptera <- gdata::drop.levels(Hemiptera)
levels(Hemiptera$Species)

# 1) "Aphis gossypii"    
Aphis_gossypii <- subset(Hemiptera, Hemiptera$Species == "Aphis gossypii")
Aphis_gossypii<- gdata::drop.levels(Aphis_gossypii)

# 2) "Corythucha ciliata"   
Corythucha_ciliata <- subset(Hemiptera, Hemiptera$Species == "Corythucha ciliata")
Corythucha_ciliata<- gdata::drop.levels(Corythucha_ciliata)

# 3)  "Planococcus citri"      
Planococcus_citri <- subset(Hemiptera, Hemiptera$Species == "Planococcus citri")
Planococcus_citri<- gdata::drop.levels(Planococcus_citri)


# 4) "Tetraneura nigri abdominalis"
Tetraneura_nigri_abdominalis <- subset(Hemiptera, Hemiptera$Species == "Tetraneura nigri abdominalis")
Tetraneura_nigri_abdominalis<- gdata::drop.levels(Tetraneura_nigri_abdominalis)

## 4 "Lepidoptera" 
Lepidoptera <- subset(summary, summary$Order == "Lepidoptera")
Lepidoptera <- gdata::drop.levels(Lepidoptera)
levels(Lepidoptera$Species)

# 1)"Cydia pomonella"
Cydia_pomonella <- subset(Lepidoptera, Lepidoptera$Species == "Cydia pomonella")
Cydia_pomonella<- gdata::drop.levels(Cydia_pomonella)


## 5 "Hymenoptera" 
Hymenoptera <- subset(summary, summary$Order == "Hymenoptera")
Hymenoptera <- gdata::drop.levels(Hymenoptera)
levels(Hymenoptera$Species)

# 1) "Euplectrus ronnai"      
Euplectrus_ronnai <- subset(Hymenoptera, Hymenoptera$Species == "Euplectrus ronnai")
Euplectrus_ronnai <- gdata::drop.levels(Euplectrus_ronnai)

# 2) "Glyptapanteles muesebecki"  
Glyptapanteles_muesebecki <- subset(Hymenoptera, Hymenoptera$Species == "Glyptapanteles muesebecki")
Glyptapanteles_muesebecki <- gdata::drop.levels(Glyptapanteles_muesebecki)

# 3) "Macrocentrus iridescens"
Macrocentrus_iridescens <- subset(Hymenoptera, Hymenoptera$Species == "Macrocentrus iridescens")
Macrocentrus_iridescens <- gdata::drop.levels(Macrocentrus_iridescens)

# 4) "Telenomus chrysopae"  
Telenomus_chrysopae <- subset(Hymenoptera, Hymenoptera$Species == "Telenomus chrysopae")
Telenomus_chrysopae <- gdata::drop.levels(Telenomus_chrysopae)

# 5) "Telenomus isis"    
Telenomus_isis <- subset(Hymenoptera, Hymenoptera$Species == "Telenomus isis")
Telenomus_isis <- gdata::drop.levels(Telenomus_isis)

# 6) "Telenomus lobatus"     
Telenomus_lobatus <- subset(Hymenoptera, Hymenoptera$Species == "Telenomus lobatus")
Telenomus_lobatus <- gdata::drop.levels(Telenomus_lobatus)

# 7) "Theocolax elegans" 
Theocolax_elegans <- subset(Hymenoptera, Hymenoptera$Species == "Theocolax elegans")
Theocolax_elegans <- gdata::drop.levels(Theocolax_elegans)

# 8) "Trichogramma bruni"      
Trichogramma_bruni <- subset(Hymenoptera, Hymenoptera$Species == "Trichogramma bruni")
Trichogramma_bruni <- gdata::drop.levels(Trichogramma_bruni)

# 9) "Trichogramma sp. nr. Lutea"  
Trichogramma_Lutea <- subset(Hymenoptera, Hymenoptera$Species == "Trichogramma sp. nr. Lutea")
Trichogramma_Lutea <- gdata::drop.levels(Trichogramma_Lutea)

# 10) "Trichogramma sp. nr. Mwanzai"
Trichogramma_Mwanzai <- subset(Hymenoptera, Hymenoptera$Species == "Trichogramma sp. nr. Mwanzai")
Trichogramma_Mwanzai <- gdata::drop.levels(Trichogramma_Mwanzai)

## 6 "Psocoptera" 
Psocoptera <- subset(summary, summary$Order == "Psocoptera")
Psocoptera <- gdata::drop.levels(Psocoptera)
levels(Psocoptera$Species)

# 1) "Lepinotus reticulatus"
Lepinotus_reticulatus <- subset(Psocoptera, Psocoptera$Species == "Lepinotus reticulatus")
Lepinotus_reticulatus<- gdata::drop.levels(Lepinotus_reticulatus)


## 7 "Prostigmata"
Prostigmata <- subset(summary, summary$Order == "Prostigmata")
Prostigmata <- gdata::drop.levels(Prostigmata)
levels(Prostigmata$Species)

# 1) "Tetranychus mcdanieli"
Tetranychus_mcdanieli <- subset(Prostigmata, Prostigmata$Species == "Tetranychus mcdanieli")
Tetranychus_mcdanieli <- gdata::drop.levels(Tetranychus_mcdanieli)


levels(summary$Order)


