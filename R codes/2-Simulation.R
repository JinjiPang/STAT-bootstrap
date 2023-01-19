
## Load packages
library(tidyverse)
library(boot)
library(pROC)
library(parallel)
library(foreach)
library(doParallel)

##Simulation demonstration

##Here we demonstrate our simulation with the Simulation Study 1, parameter 1

set.seed(2022)

##Challenge status, 0 means unchallenged, 1 means challenged
##Challenge status for simulation study 1
Challenge_status = c(rep(0, 6*40), rep(c(0, rep(1, 5)), 60))


##Simulate 2000 data under parameter setting 1 
 
sim1data1 <- lapply(1:2000, function(i) {

  data_sim <- data.frame(Exposed = Challenge_status,
                         wvELISA = -0.05636 + 1.38728*Challenge_status + rep(rnorm(100, 0, sd = 0.4879), each = 6) + rnorm(600, 0, sd = 0.5419),
                         id = rep(1:100, each = 6))

})

#######################################################

##Get the number of cores for parallel processing
cores=detectCores()

##Please run all the functions in file `Bootstrap functions` before running the following codes.

##For Mac OS machine
##Simulation parameter1+ Traditional bootstrap method

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim1data1[[i]]


  # calculate 95% confidence interval for AUC here
  ci <-calc_ci_95one(data_sim)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = cores[1]-1)

result1<-Reduce("+", cover_mc)

print(result1$cover_p/2000)
print(result1$ci_width/2000)


##Simulation parameter1 + cluster bootstrap method

cover_mc <- mclapply(1:2000, function(i) {


  data_sim <- sim1data1[[i]]

  data_sim_list <- lapply(unique(data_sim$id), function(id) {
    data_sim[data_sim$id == id, ]
  })

  # calculate 95% confidence interval here

  ci <-calc_ci_95two(data_sim_list)

  out<-data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = cores[1]-1)

result2<-Reduce("+", cover_mc)

print(result2$cover_p/2000)
print(result2$ci_width/2000)


## Simulation parameter1+ hierarchical bootstrap method

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim1data1[[i]]

  data_sim_list <- lapply(unique(data_sim$id), function(id) {
    data_sim[data_sim$id == id, ]
  })

  ci <-calc_ci_95three(data_sim_list)

  out<-data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = cores[1]-1)

result3<-Reduce("+", cover_mc)

print(result3$cover_p/2000)
print(result3$ci_width/2000)


#########################################################################

## Here we used `foreach` function for Linux or Windows operation systems.
## Simulation parameter 1 + traditional bootstrap method

cover_mc <- foreach(i=1:2000)%dopar%{
  
  data_sim <- sim1data1[[i]]
  
  # calculate 95% confidence interval for AUC here
  ci <-calc_ci_95one(data_sim)
  
  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[2])),
    ci_width = (ci[2]-ci[1])
    
  )
  
}

result1<-Reduce("+", cover_mc)

print(result1$cover_p/2000)
print(result1$ci_width/2000)


##Simulation parameter1 + cluster bootstrap method
cover_mc <- foreach(i=1:2000)%dopar% {
  
  data_sim <- sim1data1[[i]]
  
  data_sim_list <- lapply(unique(data_sim$id), function(id) {
    data_sim[data_sim$id == id, ]
  })
  
  # calculate 95% confidence interval here
  ci <-calc_ci_95two(data_sim_list)
  
  out<-data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[2])),
    ci_width = (ci[2]-ci[1])
    
  )
  
}

result2<-Reduce("+", cover_mc)

print(result2$cover_p/2000)
print(result2$ci_width/2000)


## Simulation parameter1+ hierarchical bootstrap method

cover_mc <- foreach(i=1:2000)%dopar% {
  
  data_sim <- sim1data1[[i]]
  
  data_sim_list <- lapply(unique(data_sim$id), function(id) {
    data_sim[data_sim$id == id, ]
  })
  
  ci <-calc_ci_95three(data_sim_list)
  
  out<-data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[2])),
    ci_width = (ci[2]-ci[1])
    
  )
  
}

result3<-Reduce("+", cover_mc)

print(result3$cover_p/2000)
print(result3$ci_width/2000)





