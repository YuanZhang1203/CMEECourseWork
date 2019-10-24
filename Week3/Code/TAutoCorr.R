load("../Data/KeyWestAnnualMeanTemperature.RData")
plot(ats$Year, ats$Temp, pch = 16) #plot data 
lines(ats$Year, ats$Temp) #add lines

#Compute the appropriate correlation coefficient between successive years and store it 
n = nrow(ats)
c0 = cor(ats$Temp[1:(n-1)], ats$Temp[2:n])


#Repeat this calculation 10000 times by -- randomly permuting the time series, and then recalculating the correlation coefficient for each randomly permuted year sequence and storing it.
c = NULL
for (i in 1:10000) {
    Temp = sample(ats$Temp)
    c = cbind(c, cor(Temp[1:(n-1)], Temp[2:n]))
}
sum(c>c0) / 10000
