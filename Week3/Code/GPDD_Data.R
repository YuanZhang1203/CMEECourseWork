library(maps)

load(file = "../Data/GPDDFiltered.RData")

pdf("../results/GPDD_map.pdf", 11.7, 8.3)
map("world")
points(gpdd$long, gpdd$lat, pch=3, col="red")

dev.off()
## Superimposes on the map all the locations from which we have data in the GPDD dataframe.
## The locations are mainly in the north america and europe
