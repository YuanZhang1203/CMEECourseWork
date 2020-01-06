# CMEE 2019 HPC excercises R code main proforma

name <- "Yuan Zhang"
preferred_name <- "Yuan"
email <- "yz12119@ic.ac.uk"
username <- "yz12119"
personal_speciation_rate <- 0.004963 # will be assigned to each person individually in class and should be between 0.002 and 0.007

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

# Questions 18 and 19 involve writing code elsewhere to run your simulations on the cluster

# Question 20 
process_cluster_results <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  # initialise empty vectors to store sum totals of octave data for each size
  octaves_500 = c()
  octaves_1000 = c()
  octaves_2500 = c()
  octaves_5000 = c()
  # set counters to count each octave per size
  counter_500 = 0
  counter_1000 = 0
  counter_2500 = 0
  counter_5000 = 0
  
  # generate list of results files
  files <- list.files(path = "../Results/results/", pattern = "*.rda")
  
  # set for loop over files to extract octave information
  for (i in files){
    load(paste0("cluster_run_",i,".rda")) # load each file
    octaves_tot = c() # initliase empty octave vector for i file octave sum
    counter = 0 # reset counter to 0
    
    # loop over each octave in file i
    for (j in 1:length(octaves_list)){
      if (j > (burn_in_generations/interval_oct)){ # don't inlcude octaves from burn in period
        # sum all octaves in file i
        octaves_tot = sum_vect(octaves_tot, octaves_list[[j]])
        # count all octaves in file i
        counter = counter + 1
      }
    }
    
    # set if statements to sum octaves over each size
    if (size == 500){
      octaves_500 = sum_vect(octaves_500, octaves_tot)
      # add file i count to total count for that size
      counter_500 = counter_500 + counter
    }
    if (size == 1000){
      octaves_1000 = sum_vect(octaves_1000, octaves_tot)
      counter_1000 = counter_1000 + counter
    }
    if (size == 2500){
      octaves_2500 = sum_vect(octaves_2500, octaves_tot)
      counter_2500 = counter_2500 + counter
    }
    if (size == 5000){
      octaves_5000 = sum_vect(octaves_5000, octaves_tot)
      counter_5000 = counter_5000 + counter
    }
  }
  
  # calculate mean for each octave bin per size
  octaves_means_500 = octaves_500 / counter_500
  octaves_means_1000 = octaves_1000 / counter_1000
  octaves_means_2500 = octaves_2500 / counter_2500
  octaves_means_5000 = octaves_5000 / counter_5000
  
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  par(mfrow = c(2, 2), oma=c(0,0,2.5,0)) # edit outer margin property to make space for title
  barplot(octaves_means_500, ylim = c(0,max(octaves_means_500)+1), 
          ylab = "Mean species abundance",
          xlab = "Species abundance octaves",
          main = "Community of size 500",
          col = "grey", 
          names.arg = 1:length(octaves_500)) # print octaves on x-axis
  barplot(octaves_means_1000, ylim = c(0,max(octaves_means_500)+1),
          ylab = "Mean species abundance",
          xlab = "Species abundance octaves",
          main = "Community of size 1000",
          col = "red",
          names.arg = 1:length(octaves_1000))
  barplot(octaves_means_2500, ylim = c(0,max(octaves_means_500)+1),
          ylab = "Mean species abundance",
          xlab = "Species abundance octaves",
          main = "Community of size 2500",
          col = "yellow",
          names.arg = 1:length(octaves_2500))
  barplot(octaves_means_5000, ylim = c(0,max(octaves_means_500)+1),
          ylab = "Mean species abundance",
          xlab = "Species abundance octaves",
          main = "Community of size 5000",
          col = "blue",
          names.arg = 1:length(octaves_5000))
  title(main = "Mean species abundance octaves for different community sizes", outer=TRUE) 
  
  # create list output to be returned and save as rda file
  combined_results = list(octaves_means_500, octaves_means_1000, octaves_means_2500, octaves_means_5000)
  save(combined_results, file = "../Results/yuan_cluster_results.rda")

  return(combined_results)
}

# Question 21
question_21 <- function()  {
  return(list(log(8, 3), "8 copies of itself to construct the same shape but 3 times larger of length. 3^D = 8, thereby gets the fractal dimension D"))
}

# Question 22
question_22 <- function()  {
  return(list(log(20, 3), "20 copies of itself to construct the same shape but 3 times larger of length. 3^D = 20, thereby gets the fractal dimension D"))
}

# Question 23
chaos_game <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  # a. Store the following three points that correspond to coordinates on a graph
  points = list(A = c(0,0), B = c(3,4), C = c(4,1))
  # b. Initialize the point vector X 
  X = c(0, 0)
  # c. Plot a very small point on the graph at X
  plot(X[1], X[2],cex =0.1, pch = 16, type="p", xlim=c(0, 5), ylim=c(0, 5), xlab = "X",ylab = "Y")
  # e. Write a loop to repeat the code of c. and d. 100 times 
  for (i in 1:100) {
    X = (X + points[[sample(1:3,1)]])/2 # d. Choose one of the three points (A, B or C) at random and move X half way towards whichever of the three points you chose.  
    points(X[1],X[2],cex=0.1, pch = 16)
  }
  # repeat for 1000 times
  for (i in 1:1000) {
    X = (X + points[[sample(1:3,1)]])/2 # d. Choose one of the three points (A, B or C) at random and move X half way towards whichever of the three points you chose.  
    points(X[1],X[2],cex=0.1, pch = 16)
  }  
  return("The points constrct to a fractal triangle (Sierpinski gasket). With more points formming, the distribution become denser")
}

# Question 24
turtle <- function(start_position, direction, length)  {
  endpoint = c(start_position[1] + length * cos(direction), start_position[2] + length * sin(direction))
  x = c(start_position[1], endpoint[1])
  y = c(start_position[2], endpoint[2])
  lines(x, y, type = "l")
  return(endpoint) # you should return your endpoint here.
}

# Question 25
elbow <- function(start_position, direction, length)  {
  # get the first line and the endpoint as the start point of the second line
  endpoint = turtle(start_position,direction, length)
  # then get the next point
  turtle(endpoint, direction - pi/4, 0.95*length)  #   direction = first_direction - pi/4  # length = first_length * 0.95
}

# Question 26
spiral <- function(start_position, direction, length)  {
  # get the first line and the endpoint as the start point of the second line
  endpoint = turtle(start_position,direction, length)
  # set a length limit to stop the recursion
  if (length > 0.1){
    spiral(endpoint, direction - pi/4, 0.95*length)  #   direction = first_direction - pi/4  # length = first_length * 0.95
  }  
  return("After setting the limitation, the fuction works and forms a sprial.  With this function, the lines become recursive, which forms continously a reduced length in.")
}

# Question 27
draw_spiral <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  # set the plot
  plot(1,type = "n", xlab = "x", ylab = "y",xlim = c(0,4), ylim = c(0,4),cex = 0.1, pch = 16, main = "Draw a Spiral")
  # draw out
  spiral(c(1,2), pi/3, length = 1)
}

# Question 28
tree <- function(start_position, direction, length)  {
  if (length > 0.001) {
    turtle(start_position, direction, length)
    endpoint = c(start_position[1] + length * cos(direction), start_position[2] + length * sin(direction))
    tree(endpoint, direction - pi/4, 0.65*length) # turn one side
    tree(endpoint, direction + pi/4, 0.65*length) # turn the another side
  }  
}
draw_tree <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  # set the plot
  plot(1,type = "n", xlab = "x", ylab = "y",xlim = c(0,5), ylim = c(0,4),cex = 0.1, pch = 16, main = "Draw a tree")
  # draw out
  tree(c(2,1), pi/2, 1)
}

# Question 29
fern <- function(start_position, direction, length)  {
  if (length > 0.001) {
    turtle(start_position, direction, length)
    endpoint = c(start_position[1] + length * cos(direction), start_position[2] + length * sin(direction))
    fern(endpoint, direction + pi/4, 0.38*length)
    fern(endpoint, direction , 0.87*length)
  }  
}
draw_fern <- function()  {
  graphics.off() # clear any existing graphs and plot your graph within the R window
  # set the plot
  plot(1,type = "n", xlab = "x", ylab = "y",xlim = c(0,8), ylim = c(0,10),cex = 0.1, pch = 16, main = "Furn 1")
  # draw out
  fern(c(4,1), pi/2 , 1)
}

# Question 30
fern2 <- function(start_position, direction, length, dir)  {
  if (length > 0.001) {
    turtle(start_position, direction, length)
    endpoint = c(start_position[1] + length * cos(direction), start_position[2] + length * sin(direction))
    fern2(endpoint, direction + (dir) * pi/4, 0.38 * length, dir)
    fern2(endpoint, direction, 0.87 * length, -1 * dir)
  } 
}
draw_fern2 <- function()  {
  # clear any existing graphs and plot your graph within the R window
  graphics.off() 
  # set the plot 
  plot(1,type = "n", xlab = "x", ylab = "y", xlim = c(0,8), ylim = c(0,10),cex = 0.1, pch = 16, main = "Furn 2")
  # draw out
  fern2(c(4,1), pi/2, 1, 1)
}

# Challenge questions - these are optional, substantially harder, and a maximum of 16% is available for doing them.  

# Challenge question A
Challenge_A <- function() {
  # clear any existing graphs and plot your graph within the R window
  graphics.off()
  
  # set parameters (same as Question16)
  community = init_community_max (100)
  speciation_rate = 0.1
  duration = 200
  repeats = 60 # set a number as repeats, could be larger
  
  # get the 200 generations 
  mat_max = replicate(repeats,neutral_time_series_speciation(community = init_community_max(100), speciation_rate = 0.1, duration))
  mat_min = replicate(repeats,neutral_time_series_speciation(community = init_community_min(100), speciation_rate = 0.1, duration))
  
  # store means species richness 
  mean_max = matrix(nrow = 0, ncol = duration)
  mean_min = matrix(nrow = 0, ncol = duration)
  for(i in 1:duration) 
    {mean_max[i] = mean(mat_max[,i])}
  for(i in 1:duration)
    {mean_min[i] = mean(mat_min[,i])}
  
  # 97.2% confidence intervals, error = qnorm(0.975)*s/sqrt(n)
  errorMax = rep(NA, ncol(length(mat_max)))
  for(i in 1:ncol(mat_max)) {errorMax[i] = qnorm(0.972)*(sd(mat_max[,i])/sqrt(nrow(mat_max)))
  errorMaxright = mean_max + errorMax
  errorMaxleft = mean_max- errorMax}
  
  errorMax = rep(NA, ncol(mat_max))
  for(i in 1:ncol(mat_min)) {errorMax[i] = qnorm(0.972)*(sd(mat_min[,i])/sqrt(nrow(mat_min)))
  errorMaxright = mean_min + errorMax
  errorMaxleft = mean_min - errorMax}
  
  # plot for community with maximum initial diversity
  plot(x = 0:duration,y = rowSums(mat_max,na.rm=T)/repeats, xlim = c(0,200), ylim = c(0,100),type = "l",col = "red",
       ylab = "Species Richness", xlab = "Generations",
       main = "Mean Species Richness with 97.2% Confidence Interval")
  lines(x = 0:duration,y = rowSums(mat_min,na.rm=T)/repeats,col = "blue")
  # plot Confidence Interval
  x = 
  polygon(c(seq(0:60), rev(seq(60:200))),c(errorMaxleft,rev(errorMaxright)), col = maxRed, border = FALSE) 
  polygon(c(seq(duration)),c(errorMinleft,rev(errorMinright)), col = minBlue, border = FALSE) 
  
  # add the estimated number of generations needed for system to reach dynamic equilibrium
  abline(v = 30, col = "yellow", lwd = 1, lty = 2)
  text(100, 20, "Estimated dynamic equilibrium", cex = 0.75)
  
  # add legend to plot
  legend("topright", legend = c("Maxi Initial Community", "Min Initial Community"),
         col = c("red", "blue"),lty = 1,cex = 0.75,bty = "n")
}

# Challenge question B
Challenge_B <- function() {
  # clear any existing graphs and plot your graph within the R window
  
}

# Challenge question C
Challenge_C <- function() {
  # clear any existing graphs and plot your graph within the R window
}

# Challenge question D
Challenge_D <- function() {
  # clear any existing graphs and plot your graph within the R window
  return("type your written answer here")
}

# Challenge question E
Challenge_E <- function() {
  # clear any existing graphs and plot your graph within the R window
  return("type your written answer here")
}

# Challenge question F
Challenge_F <- function() {
  # clear any existing graphs and plot your graph within the R window
  return("type your written answer here")
}

# Challenge question G should be written in a separate file that has no dependencies on any functions here.


