#using loops, one operation that is slow in R (and somewhat slow in all languages) is memory allocation for a particular variable that will change during loping
a <- NA
for (i in 1:10) {
  a <- c(a, i)
  print(a)
  print(object.size(a))
}


#add Pre-allocation -- more faster
a <- rep(NA, 10)

for (i in 1:10) {
  a[i] <- i
  print(a)
  print(object.size(a))
}