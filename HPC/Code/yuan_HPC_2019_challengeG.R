# CMEE 2019 HPC excercises R code challenge G proforma

rm(list=ls()) # nothing written elsewhere should be needed to make this work

name <- "Yuan Zhang"
preferred_name <- "Yuan"
email <- "yz12119@ic.ac.uk"
username <- "yz12119"


# don't worry about comments for this challenge - the number of characters used will be counted starting from here
plot.new();f = function(s,d,l,o){e=s+c(cos(d),sin(d))*l;lines(rbind(s,e));if(l>0.001){f(e,d,l*0.87,-o);f(e,d+o*pi/4,l*0.38,o)}};f(c(0.5,0),pi/2,0.1,1)


