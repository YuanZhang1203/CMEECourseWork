######### Functions ##########

## A function to take a sample of size n from a population "popn" and return its mean
myexperiment <- function(popn,n){
    pop_sample <- sample(popn, n, replace = FALSE)
    return(mean(pop_sample))
}

n <- 20 # sample size for each experiment
num <- 1000 # Number of times to rerun the experiment

## Calculate means using a for loop without preallocation:
loopy_sample1 <- function(popn, n, num){
    result1 <- vector() #Initialize empty vector of size 1 
    for(i in 1:num){
        result1 <- c(result1, myexperiment(popn, n))
  }
    return(result1)
}
print("The loopy, non-preallocation approach takes:" )
print(system.time(loopy_sample1(popn, n, num)))

##2.To run "num" iterations of the experiment using a for loop on a vector with preallocation:
loopy_sample2 <- function(popn, n, num){
    result2 <- vector(, n) #Preallocate expected size
    for(i in 1:num){
        result2[i] <- myexperiment(popn, n)
  }
    return(result2)
}
print("The loopy, but with preallocation on a vector approach takes:" )
print(system.time(loopy_sample2(popn, n, num)))

##3.To run "num" iterations of the experiment using a for loop on a list with preallocation:
loopy_sample3 <- function(popn, n, num){
    result3 <- vector("list", num) #Preallocate expected size
    for(i in 1:num){
        result3[[i]] <- myexperiment(popn, n)
  }
    return(result3)
}
print("The loopy, but with preallocation on a list approach takes:" )
print(system.time(loopy_sample2(popn, n, num)))

## 4.To run "num" iterations of the experiment using vectorization with sapply:
lapply_sample <- function(popn, n, num){
    result4 <- sapply(1:num, function(i) myexperiment(popn, n))
    return(result4)
}
print("The vectorized sapply approach takes:" )
print(system.time(sapply_sample(popn, n, num)))

## 5.To run "num" iterations of the experiment using vectorization with lapply:
sapply_sample <- function(popn, n, num){
    result5 <- lapply(1:num, function(i) myexperiment(popn, n))
    return(result5)
}
print("The vectorized lapply approach takes:" )
print(system.time(lapply_sample(popn, n, num)))
