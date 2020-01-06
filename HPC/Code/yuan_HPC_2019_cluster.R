# CMEE 2019 HPC excercises R code HPC run code proforma
# personal_speciation_rate <- 0.004963 

rm(list=ls()) # good practice 
graphics.off() # Clear workspace and turn off graphics

# Load all the functions by sourcing my main R file
#### PATH for testing locally 
# source("CMEECourseWork/HPC/Code/yuan_HPC_main_2019.R") 

# source("yuan_HPC_main_2019.R")  ##### this step cannot be succussful in the cx1, so I have to paste the codes

###################################### paste the Questions in yuan_HPC_main_2019.R (The content of cluster is start from line 254)

# Question 1
species_richness <- function(community){
  species_richness = (unique(community)) #remove the repeated elements
  return(length(species_richness)) # statistics of the number of richness
}

# Question 2
init_community_max <- function(size){
  return(seq(1, size, by = 1))  #list the number from 1 to size in the vector
}

# Question 3
init_community_min <- function(size){
  return(seq(1, 1, length.out = size)) # reapeat 1 with length of size
}

# Question 4
choose_two <- function(max_value){
  uniform_distribution = seq(1, max_value, by = 1) # uniform distribution between 1 and max
  two <- sample(uniform_distribution, 2, replace = FALSE, prob = NULL) # get two random numbers 
  return(two)
}

# Question 5
neutral_step <- function(community){
  order = choose_two(length(community)) # get the two random order numbers from the community vector
  community[order[1]] = community[order[2]] # exchange two numbers selected 
  return(community)
}

# Question 6
neutral_generation <- function(community){
  n = length(community)
  m = ifelse(n %% 2 == 0, n / 2, (n + sample(c(-1,1))) / 2) # x/2; If x is not an even number, choose at random whether to round up or down to the nearest whole number to determine generation length. Then divided by 2  
  i = 1
  for (i in 1:m){ 
    community = neutral_step(community) # the loop for the neutral generation 
  }
  return(community)
}

# Question 7
neutral_time_series <- function(community,duration)  {
  result = c(species_richness(community)) # define the species richness of the community
  for (i in 1:duration){
    community =  neutral_generation(community) 
    species_richness = species_richness(community)
    result = c(result, species_richness) # combine a time series of species richness  
  }
  return(result)
}

# Question 8
question_8 <- function() {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  y = c(neutral_time_series (community = init_community_max (100), duration = 200)) #set y
  x = 1: length(y)  #set x 
  plot(x, y, type = "l", pch = 16, cex = 0.5,
       col= "red", 
       xlab = "Generations", 
       ylab = "Speices Richness",
       main = "Neutral model simulation") 
  return("If waiting long enough, The state of the system always converge to one species with richness of 1. The reason is the selection by the nature with no new species invadation, and finally one species will be the domination.")
}

# Question 9
neutral_step_speciation <- function(community, speciation_rate)  {
  x = runif(1, min = 0, max = 1) # get a random number between 0 and 1
  if(x > speciation_rate){
    community = neutral_step(community) #keep the original status
  } else {
    new_species = max(community) + 1 # guarantee to get a number to represent the new species that is different from any of the species numbers
    order = choose_two(length(community)) # get the two random order numbers from the community vector
    community[order[1]] = new_species # exchange two numbers 
  }
  return(community)
}

# Question 10
neutral_generation_speciation <- function(community,speciation_rate)  {
  n = length(community)
  m = ifelse(n %% 2 == 0, n / 2, (n + sample(c(-1,1))) / 2) # similar as Question 6
  i = 1
  for (i in 1:m){ 
    community = neutral_step_speciation(community, speciation_rate) # the loop for the neutral generation speciation
  }
  return(community)
}

# Question 11
neutral_time_series_speciation <- function(community,speciation_rate,duration)  {
  result = c(species_richness(community)) # define the species richness of the community
  for (i in 1:duration){
    community =  neutral_step_speciation(community, speciation_rate)
    species_richness = species_richness(community) 
    result = c(result, species_richness) # combine a time series of species richness  
  }
  return(result)  
}

# Question 12
question_12 <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  y1 = c(neutral_time_series_speciation (community = init_community_max (100), speciation_rate = 0.1, duration = 200)) #set y1 with initial states given by init_community_max
  y2 = c(neutral_time_series_speciation (community = init_community_min (100), speciation_rate = 0.1, duration = 200)) #set y2 with initial states given by init_community_min
  x = 1: length(y1)  #set x 
  plot(x, y1, type = "l", pch = 16, cex = 0.5, # get the basic plot
       col= "red", 
       xlab = "Generations", 
       ylab = "Speices Richness", 
       main = "A neutral model simulation with speciation")
  lines(x, y2, col= "blue", pch = 16, cex = 0.5) # aid another series
  legend("topright",c("init_community_max","init_community_min"), # set legend to distinguish two series
         col=c("red","blue"), lty =1, lwd =1)
  return("From this plot, I find that with different initial conditions, whose impact on the soecies richness are similar after long-time generations. The result of the species richness is fluctuated but stable within in a range, which is a dynamic equilibrium. 
          The reason might is that with long-time generations, the posibilty of speciation and extinction would be similar.Those are the limit conditions for species rishness and decide the state.")
}

# Question 13
species_abundance <- function(community)  {
  species_abundance = sort(as.numeric(table(community)), decreasing=T)
  return(species_abundance)
}

# Question 14
octaves <- function(abundance_vector) {
  log_abundance = log(abundance_vector, base = 2) # get the log2 number of abundance
  integer_log_abundance = floor(log_abundance) + 1 # convert to intergers  
  result = tabulate(integer_log_abundance) # calculate the number of occurrences of each integer in bin 
  return(result)
}

# Question 15
sum_vect <- function(x, y) {
  if (length(x) > length(y)){ #set length(y) >= length(x); otherwise, exchange x and y
    a = x
    x = y
    y = a
  }
  difference = length(y) - length(x) #count the number of the gap of x and y 
  if (length(x) != length(y)){
    x = c(x, rep(0, difference)) #add 0 to let x have a same length as y
    sum = x + y
  }else{
    sum = x + y
  }
  return(sum)
}

# Question 16 
question_16 <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  
  # initially, get the 200 generations
  for (i in 1:200) {
    com_max = neutral_generation_speciation(community = init_community_max (100), speciation_rate = 0.1)
    com_min = neutral_generation_speciation(community = init_community_min (100), speciation_rate = 0.1) 
    oct_max = octaves(species_abundance(com_max))
    oct_min = octaves(species_abundance(com_max))
  }                
  # get a further 2000 generations
  duration = 2000
  record_every = 20
  list_max = list() # set list
  list_min = list() # set list
  for (k in 1:2000){
    com_max = neutral_generation_speciation(community = com_max, speciation_rate = 0.1)
    com_min = neutral_generation_speciation(community = com_min, speciation_rate = 0.1)
    list_max = c(list_max, list(com_max))
    list_min = c(list_min, list(com_min)) 
    if (k %% 20 == 0){ # record the species abundance octave vector every 20 generations
      oct_max = sum_vect(oct_max, octaves(species_abundance(list_max[k])))
      oct_min = sum_vect(oct_min, octaves(species_abundance(list_min[k])))
    }
  }  
  
  # get the everage 
  ave_max = oct_max/100 # average species abundance distribution (as octaves) with initial community of init_community_max (100) 
  ave_min = oct_min/100 # average species abundance distribution (as octaves) with initial community of init_community_min (100)  
  
  # consider that the length of the ave_max and ave_min may be different 
  if (length(ave_max) > length(ave_min)){ #set length(ave_min) >= length(ave_max); otherwise, exchange them
    a = ave_max
    ave_max = ave_min
    ave_min = a
  }
  difference = length(ave_min) - length(ave_max) #count the number of the gap of them 
  if (length(ave_max) != length(ave_min)){
    ave_max = c(ave_max, rep(0, difference)) #add 0 to let ave_max have same length as ave_min
  }else{
    ave_max = ave_max
  }
  
  # plot a bar chart of the average species abundance distribution (as octaves)
  data = rbind(ave_max, ave_min) # let a and b store within a matrix
  seq = seq(1:length(ave_max)) # get the n of octaves
  barplot(data, offset = 0, axis.lty = 1, col = c("red", "blue"), beside = TRUE,
          main= "Averge Species Abundance Distribution ",
          xlab = "Octave", 
          ylab = "Species Number",
          names.arg = seq)
  box()
  legend("topright",c("Max","Min"),fill=c("red","blue")) # set legend to distinguish two series
  
  return("The initial condition of the system seems not matter.
          The resson is population size and speciation rate decide the final species abundance. After a long-time generations, the ecosystem will get a dynamic equilibrium. Besdies, the abundence of species are low, and dominant species will be dynamically stable.")
}

# Question 17
cluster_run <- function(speciation_rate, size, wall_time, interval_rich, interval_oct, burn_in_generations, output_file_name)  {
  # set parameters 
  community = init_community_min(size) # Start with a community with size given by the input size and minimal diversity
  t0 = proc.time()[3] # set the initial time
  octaves_list = list()  # use a list to store species abundances as octaves 
  time_series = list() # use a list to store the species richnesses
  # within the wall_time:
  while((proc.time()[3] - t0)/60 <  wall_time){
    for (i in 1:burn_in_generations){ 
      community = neutral_generation_speciation(community, speciation_rate) # apply neutral generations
      if (i %% interval_rich == 0){
        time_series = c(time_series, species_richness(community)) # record the species richness
      }
      if (i %% interval_oct == 0){
        octaves_list = c(octaves_list, list(octaves(species_abundance(community)))) # record the octave
      }
    }
  }  
  totaltime = as.numeric(proc.time()[3]-t0)
  # save  results in a file
  save(time_series,octaves_list,totaltime,community, speciation_rate,size,wall_time,interval_rich,interval_oct,burn_in_generations, file = output_file_name)
}


###################################### paste the Questions in yuan_HPC_main_2019.R
 

# set new variable
iter <- as.numeric(Sys.getenv("PBS_ARRAY_INDEX")) 
#### for testing locally
# iter = sample(1:100,1)

# set seed
set.seed(iter)

# choose community size
community_size = c(500,1000,2500,5000) # set values for community size in simulations (500,1000,2500,5000) 
size = community_size[iter %% 4 + 1]

# run simulation
cluster_run(speciation_rate = 0.004963, size = size, wall_time = 11.5 * 60, interval_rich = 1, interval_oct = size/10,
            burn_in_generations = 8*size, output_file_name = paste0("cluster_run_",iter,".rda"))


