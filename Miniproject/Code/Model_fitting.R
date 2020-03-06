#############################################################################
# Title: Model fitting in R
# CMEE MSc March 2020 
# Author: YUAN ZHANG 
#############################################################################

rm(list = ls())
graphics.off()

#load packages
library("ggplot2")
library("minpack.lm")
library("dplyr")

# input data 
modified_data <- read.csv("../Data/modified_data.csv")


######################### set models #######################
#### set the function of the classic logistic equation
logistic_model <- function(t, r_max, N_max, N_0){ # The classic logistic equation
  return(N_0 * N_max * exp(r_max * t)/(N_max + N_0 * (exp(r_max * t) - 1)))
}
# Nt is population size at time t 
# N0 is initial population size
# r is maximum growth rate (AKA rmax)
# Nmax is carrying capacity (commonly denoted by K in the ecological literature).


#### set other models #### 
# For added complication, note that these are written in log to the base 10
gompertz_model <- function(t, r_max, N_max, N_0, t_lag){ # Modified gompertz growth model (Zwietering 1990)
  return(N_0 + (N_max - N_0) * exp(-exp(r_max * exp(1) * (t_lag - t)/((N_max - N_0) * log(10)) + 1)))
}

#A is expressed in terms of N_max and N_0 for simplicity in obtaining starting values
# Here maximum growth rate (rmax) is the tangent to the inflection point
# t_lag is the x-axis intercept to this tangent (duration of the delay before the population starts growing exponentially) 
# A is the asymptote (A=ln(NmaxN0))
# N0 is initial cell culture (Population) density 
# Nmax is maximum population density (aka "carrying capacity")

baranyi_model <- function(t, r_max, N_max, N_0, t_lag){  # Baranyi model (Baranyi 1993)
  return(N_max + log10((-1+exp(r_max*t_lag) + exp(r_max*t))/(exp(r_max*t) - 1 + exp(r_max*t_lag) * 10^(N_max-N_0))))
}


# introduces a new dimensionless parameter h0; The length of the lag phase is determined by the value of h0 at inoculation and the post-inoculation environment.
# In this model, rmax and h0 can be related to obtain the lag time, t_lag

buchanan_model <- function(t, r_max, N_max, N_0, t_lag){ # Buchanan model - three phase logistic (Buchanan 1997)
  return(N_0 + (t >= t_lag) * (t <= (t_lag + (N_max - N_0) * log(10)/r_max)) * r_max * (t - t_lag)/log(10) + (t >= t_lag) * (t > (t_lag + (N_max - N_0) * log(10)/r_max)) * (N_max - N_0))
}

# three-phase logistic model
# Here, tmax is the time at which Nmax is reached. This model makes three assumptions:
# 1) growth rate during lag phase is zero,
# 2) growth rate during exponential phase is constant,
# 3) growth rate during stationary phase is zero.



######################### fitting models ######################### 
ID_unique <- unique(modified_data$ID) #use a loop for different IDs

# set a data frame to collect results 
stat <- data.frame(ID= character(), AIClogistic= numeric(), BIClogistic= numeric(), AICgompertz= numeric(), BICgompertz= numeric(),  AICbaranyi= numeric(),  BICbaranyi= numeric(), AICbuchanan= numeric(), BICbuchanan= numeric(), stringsAsFactors = FALSE)

for (i in 1: length(ID_unique) ){ 
  #### initial parameters ####
  data <- modified_data[which(modified_data$ID == ID_unique[i]),]
  ID <- as.character(unique(data$ID))
  
  data$Log10N <- log10(data$N)
  
  N_0_start <- min(data$Log10N)
  N_max_start <- max(data$Log10N)
  
  # use lineat model to get the max growth rate 
  lm <- lm(data$Log10N ~ data$t, data)
  # r_max_start <- as.numeric(lm$coefficients[2])
  r_max_start <- abs(max(diff(data$Log10N) / mean(diff(data$t)))) # As I get the log10(N), some max log10N might be negative, which needs to be transformed to absolute value
  
  
  t_lag_start <- data$t[which.max(diff(diff(data$Log10N)))]
  
  
  #### fit modelss ####
  fit_logistic <- try(nlsLM(Log10N ~ logistic_model(t = t, r_max, N_max, N_0), data, list(r_max=r_max_start, N_0 = N_0_start, N_max = N_max_start), control = nls.lm.control(maxiter = 100)), silent=T)
                      
  # maxiter used for positive integer. Termination occurs when the number of iterations reaches maxiter
  
  fit_gompertz <- try(nlsLM(Log10N ~ gompertz_model(t = t, r_max, N_max, N_0, t_lag), data, list(t_lag=t_lag_start, r_max=r_max_start, N_0 = N_0_start, N_max = N_max_start), control = nls.lm.control(maxiter = 100)), silent=T)

  fit_baranyi <- try(nlsLM(Log10N ~ baranyi_model(t = t, r_max, N_max, N_0, t_lag), data, list(t_lag=t_lag_start, r_max=r_max_start, N_0 = N_0_start, N_max = N_max_start), control = nls.lm.control(maxiter = 100)), silent=T)

  fit_buchanan <- try(nlsLM(Log10N ~ buchanan_model(t = t, r_max, N_max, N_0, t_lag), data, list(t_lag=t_lag_start, r_max=r_max_start, N_0 = N_0_start, N_max = N_max_start), control = nls.lm.control(maxiter = 100)), silent=T)
  
  
  #### statistical analysis of models fitting and getting the plotting points
  # get the timepoints
  timepoints <- seq(min(data$t), max(data$t), 0.5)
  
  # write loops  
  # 1 logistic model
  if(class(fit_logistic) != "try-error"){ 
    # get the statistic data
    AIClogistic <- AIC(fit_logistic)
    BIClogistic <- BIC(fit_logistic)
    
    # get the plotting points
    logistic_points <- logistic_model(t = timepoints, r_max = coef(fit_logistic)["r_max"], N_max = coef(fit_logistic)["N_max"], N_0 = coef(fit_logistic)["N_0"])
    df1 <- data.frame(ID, timepoints, logistic_points)
    df1$Model <- "Logistic"
    names(df1) <- c("ID", "t", "Log10N", "Model")
    
  } else { # set NA data
    AIClogistic <- "NA"
    BIClogistic <- "NA"
    
    df1 <- data.frame(ID, timepoints = NA, logistic_points = NA)
    df1$Model <- "Logistic"
    names(df1) <- c("ID", "t", "Log10N", "Model")
  }
  
  # 2 gompertz model
  if(class(fit_gompertz) != "try-error"){ 
    # get the statistic data
    AICgompertz <- AIC(fit_gompertz)
    BICgompertz <- BIC(fit_gompertz)
    
    # get the plotting points
    gompertz_points <- gompertz_model(t = timepoints, r_max = coef(fit_gompertz)["r_max"], N_max = coef(fit_gompertz)["N_max"], N_0 = coef(fit_gompertz)["N_0"], t_lag = coef(fit_gompertz)["t_lag"])
    df2 <- data.frame(ID, timepoints, gompertz_points)
    df2$Model <- "Gompertz"
    names(df2) <- c("ID", "t", "Log10N", "Model")
    
  } else { # set NA data
    AICgompertz <- "NA"
    BICgompertz <- "NA"
    
    df2 <- data.frame(ID, timepoints = NA, gompertz_points = NA)
    df2$Model <- "Gompertz"
    names(df2) <- c("ID", "t", "Log10N", "Model")
  }
  
  #3 baranyi model
  if(class(fit_baranyi) != "try-error"){ 
    # get the statistic data
    AICbaranyi <- AIC(fit_baranyi)
    BICbaranyi <- BIC(fit_baranyi)
    
    # get the plotting points
    baranyi_points <- baranyi_model(t = timepoints, r_max = coef(fit_baranyi)["r_max"], N_max = coef(fit_baranyi)["N_max"], N_0 = coef(fit_baranyi)["N_0"], t_lag = coef(fit_baranyi)["t_lag"])    
    df3 <- data.frame(ID, timepoints, baranyi_points)
    df3$Model <- "Baranyi"
    names(df3) <- c("ID", "t", "Log10N", "Model")
    
  } else { # set NA data
    AICbaranyi <- "NA"
    BICbaranyi <- "NA"
    
    df3 <- data.frame(ID, timepoints = NA, baranyi_points = NA)
    df3$Model <- "Baranyi"
    names(df3) <- c("ID", "t", "Log10N", "Model")
  }
  
  # 4 buchanan model
  if(class(fit_buchanan) != "try-error"){ 
    # get the statistic data
    AICbuchanan <- AIC(fit_buchanan)
    BICbuchanan <- BIC(fit_buchanan)
    
    # get the plotting points
    buchanan_points <- buchanan_model(t = timepoints, r_max = coef(fit_buchanan)["r_max"], N_max = coef(fit_buchanan)["N_max"], N_0 = coef(fit_buchanan)["N_0"], t_lag = coef(fit_buchanan)["t_lag"])
    df4 <- data.frame(ID, timepoints, buchanan_points)
    df4$Model <- "Buchanan"
    names(df4) <- c("ID", "t", "Log10N", "Model")
    
  } else { # set NA data
    AICbuchanan <- "NA"
    BICbuchanan <- "NA"
    
    df4 <- data.frame(ID, timepoints = NA, buchanan_points = NA)
    df4$Model <- "Buchanan"
    names(df4) <- c("ID", "t", "Log10N", "Model")
  }
  
  
  #collect results 
  stat_vector <- c(ID, AIClogistic, BIClogistic, AICgompertz, BICgompertz,  AICbaranyi,  BICbaranyi, AICbuchanan, BICbuchanan)
  stat[i, ] <- stat_vector
  
  plot <- rbind(df1, df2, df3, df4)
  
  # export results   
  # write.csv(x = stat, file = paste0("../Data/statistic_data/statistic_data",i,".csv"))
  
  # get model plotting for respective IDs
  if (sum(is.na(plot$t)) != 4){ # check if noun model fitted: if the sum of NA equals to 4, there is no data in the plot.csv
    ggplot(data, aes(x = t, y = Log10N)) +
      geom_point(size = 2) +
      geom_line(data = plot, aes(x = t, y = Log10N, col = Model), size = 0.5) + # plot model fittings
      theme_bw(base_size  = 16) + 
      theme(aspect.ratio = 1) + 
      labs( x = ("Time (Hours)"), y = paste("Log10 of Population Biomass")) 
    
    ggsave(paste("../Results/respective_plots/", i, ":", ID,".png", sep = ""), device = png()) # save the respective plots with unique ID 
    dev.off() 
  }
  

  print(paste0("get results for ",i))

}


write.csv(stat, file = "../Data/statistic_data.csv")











