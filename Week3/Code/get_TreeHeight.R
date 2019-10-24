library(dplyr)
args <- commandArgs()
InputFileName=args[6]
OutputFileName=gsub("^.*/", "", InputFileName)
OutputFileName=gsub("\\.csv", "", OutputFileName)

TreeHeight <- function(degrees, distance){
    radians <- degrees * pi / 180
    height <- distance * tan(radians)
    print(paste("Tree height is:", height))
    
    return (height)
}

TreeHeight2 = Vectorize(TreeHeight, vectorize.args = c('degrees','distance'))
trees = read.csv(InputFileName, header = T, sep=",", stringsAsFactors = F)
trees = trees %>% mutate(Tree.Height.m = TreeHeight2(Angle.degrees, Distance.m))
write.csv(trees, file = paste("../results/", OutputFileName, "_treeheights.csv",sep=''), row.names = F)