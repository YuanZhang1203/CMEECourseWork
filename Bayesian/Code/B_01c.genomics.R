#################################################
#B_01c.genomicsin R
# CMEE MSc
# 24 Feb 2020 Mon 
# Author: YUAN ZHANG 
#################################################

source("CMEECourseWork/Bayesian/Materials/Notebooks/math/Data/functions.R")

#### For instance, assuming that your sequence is AAGAGGA, your alleles are A and G (meaning that you want to calculate the likelihood for genotypes {AA,AG,GG}, and your sequencing error rate is 0.05, then the likelihood (not in logarithms) for each genotype is given by calcGenoLikes("AAGAGGA", "A", "G", 0.05, FALSE)
#### Using Bayes' theorem, write the formula for the posterior probability of genotype G being AA given the sequencing data D. Write the explicit denominator assuming that your alleles are A and G and all possible genotypes are only AA, AG, GG.

#b
likes <- calcGenoLikes("AAGAGGA", "A", "G", 0.05, FALSE)
print(likes)
prior <- c(1/3,1/3,1/3)
num <-likes * prior
print(num)
post <- num/sum(num)
print(post)
