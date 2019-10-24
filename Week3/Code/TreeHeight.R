library(dplyr)

# This function calculates heights of trees given distance of each tree 
# from its base and angle to its top, using  the trigonometric formula 
#
# height = distance * tan(radians)
#
# ARGUMENTS
# degrees:   The angle of elevation of tree
# distance:  The distance from base of tree (e.g., meters)
#
# OUTPUT
# The heights of the tree, same units as "distance"

TreeHeight <- function(degrees, distance){
  radians <- degrees * pi / 180
  height <- distance * tan(radians)
  print(paste("Tree height is:", height))
  
  return (height)
}

TreeHeight(37, 40)


# loads tree and create output
TreeHeight2 = Vectorize(TreeHeight, vectorize.args = c('degrees','distance'))
trees = read.csv("../data/trees.csv",sep=",",header = T, stringsAsFactors = F)
trees = trees %>% mutate(Tree.Height.m = TreeHeight2(Angle.degrees, Distance.m))
write.csv(trees, file = "../results/TreeHts.csv")





