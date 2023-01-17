library(boot)
library(pROC)


Exposed = c(rep(0, 6*40), rep(1, 6*60))

set.seed(2022)

simdataa <- lapply(1:2000, function(i) {

  data_sim <- data.frame(Exposed = Exposed,
                         wvELISA = -0.05636 + 1.38728*Exposed + rep(rnorm(100, 0, sd = 0.4879), each = 6) + rnorm(600, 0, sd = 0.5419),
                         id = rep(1:100, each = 6))

})

simdatab <- lapply(1:2000, function(i) {

  data_sim <- data.frame(Exposed = Exposed,
                         wvELISA = -0.05636 + 0.69364*Exposed + rep(rnorm(100, 0, sd = 0.4879), each = 6) + rnorm(600, 0, sd = 0.5419),
                         id = rep(1:100, each = 6))

})

simdatac <- lapply(1:2000, function(i) {

  data_sim <- data.frame(Exposed = Exposed,
                         wvELISA = -0.05636 + 1.38728*Exposed + rep(rnorm(100, 0, sd = 0.1), each = 6) + rnorm(600, 0, sd = 0.7222),
                         id = rep(1:100, each = 6))

})

simdatad <- lapply(1:2000, function(i) {

  data_sim <- data.frame(Exposed = Exposed,
                         wvELISA = -0.05636 + 0.69364*Exposed + rep(rnorm(100, 0, sd = 0.1), each = 6) + rnorm(600, 0, sd = 0.7222),
                         id = rep(1:100, each = 6))

})

simdatae <- lapply(1:2000, function(i) {

  data_sim <- data.frame(Exposed = Exposed,
                         wvELISA = -0.05636 + 1.38728*Exposed + rep(rnorm(100, 0, sd = 0.7222), each = 6) + rnorm(600, 0, sd = 0.1),
                         id = rep(1:100, each = 6))

})

simdataf <- lapply(1:2000, function(i) {

  data_sim <- data.frame(Exposed = Exposed,
                         wvELISA = -0.05636 + 0.69364*Exposed + rep(rnorm(100, 0, sd = 0.7222), each = 6) + rnorm(600, 0, sd = 0.1),
                         id = rep(1:100, each = 6))

})


## simulation parametera+method1(pROC package)

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- simdataa[[i]]

  roc<-roc(Exposed~wvELISA,data=data_sim)
  # calculate 95% confidence interval for AUC here
  ci<-ci.auc(roc, conf.level=0.95, method=c( "bootstrap"), boot.n = 1000)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[3])),
    ci_width = (ci[3]-ci[1])

  )

}, mc.cores = 23)

resulta1<-Reduce("+", cover_mc)

print(resulta1$cover_p/2000)
print(resulta1$ci_width/2000)


##########################


cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- simdataa[[i]]


  # calculate 95% confidence interval for AUC here
  ci <-calc_ci_95one(data_sim)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)

resulta11<-Reduce("+", cover_mc)

print(resulta11$cover_p/2000)
print(resulta11$ci_width/2000)




#####simulation parameter1 +method 2

cover_mc <- mclapply(1:2000, function(i) {


  data_sim <- simdataa[[i]]

  data_sim_list <- lapply(unique(data_sim$id), function(id) {
    data_sim[data_sim$id == id, ]
  })

  # calculate 95% confidence interval here

  ci <-calc_ci_95two(data_sim_list)

  out<-data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = cores[1] - 1)

resulta2<-Reduce("+", cover_mc)

print(resulta2$cover_p/2000)
print(resulta2$ci_width/2000)



#########################

# parameter1+method3

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- simdataa[[i]]

  data_sim_list <- lapply(unique(data_sim$id), function(id) {
    data_sim[data_sim$id == id, ]
  })

  ci <-calc_ci_95three(data_sim_list)

  out<-data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = cores[1] - 1)

resulta3<-Reduce("+", cover_mc)

print(resulta3$cover_p/2000)
print(resulta3$ci_width/2000)



########################simudatab


#####parameterb+method1
cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- simdatab[[i]]

  roc<-roc(Exposed~wvELISA,data=data_sim)

  ci<-ci.auc(roc, conf.level=0.95, method=c( "bootstrap"), boot.n = 1000)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.7494095 & 0.7494095< ci[3])),
    ci_width = (ci[3]-ci[1])

  )

}, mc.cores = 23)


resultb1<-Reduce("+", cover_mc)

print(resultb1$cover_p/2000)
print(resultb1$ci_width/2000)


######## b+method 1(our code)

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- simdatab[[i]]


  # calculate 95% confidence interval for AUC here
  ci <-calc_ci_95one(data_sim)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.7494095 & 0.7494095< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)

resultb11<-Reduce("+", cover_mc)

print(resultb11$cover_p/2000)
print(resultb11$ci_width/2000)


#####parameterb +method2

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- simdatab[[i]]

  data_sim_list <- lapply(unique(data_sim$id), function(id) {
    data_sim[data_sim$id == id, ]
  })

  # calculate 95% confidence interval here

  ci <-calc_ci_95two(data_sim_list)

  out<-data.frame(
    cover_p = as.numeric((ci[1] <0.7494095 & 0.7494095< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)


resultb2<-Reduce("+", cover_mc)

# results divided by total simulation times will be the final results.

print(resultb2$cover_p/2000)
print(resultb2$ci_width/2000)




#####parameterb +method3


cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- simdatab[[i]]

  data_sim_list <- lapply(unique(data_sim$id), function(id) {
    data_sim[data_sim$id == id, ]
  })

  # calculate 95% confidence interval here

  ci <-calc_ci_95three(data_sim_list)

  out<-data.frame(
    cover_p = as.numeric((ci[1] <0.7494095 & 0.7494095< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)


resultb3<-Reduce("+", cover_mc)


print(resultb3$cover_p/2000)

print(resultb3$ci_width/2000)


############################



#################
#parameterc+method1

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- simdatac[[i]]

  roc<-roc(Exposed~wvELISA,data=data_sim)
  # calculate 95% confidence interval for AUC here
  ci<-ci.auc(roc, conf.level=0.95, method=c( "bootstrap"), boot.n = 1000)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[3])),
    ci_width = (ci[3]-ci[1])

  )

}, mc.cores = 23)



resultc1<-Reduce("+", cover_mc)

print(resultc1$cover_p/2000)
print(resultc1$ci_width/2000)


#########################parameterc, method1(our code)


cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- simdatac[[i]]

  # calculate 95% confidence interval for AUC here
  ci <-calc_ci_95one(data_sim)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)


resultc11<-Reduce("+", cover_mc)

print(resultc11$cover_p/2000)
print(resultc11$ci_width/2000)


######################
###parameterc+method2


cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- simdatac[[i]]

  data_sim_list <- lapply(unique(data_sim$id), function(id) {
    data_sim[data_sim$id == id, ]
  })

  # calculate 95% confidence interval here

  ci <-calc_ci_95two(data_sim_list)

  out<-data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)


resultc2<-Reduce("+", cover_mc)

# results divided by total simulation times will be the final results.

print(resultc2$cover_p/2000)
print(resultc2$ci_width/2000)



#############################
##parameterc+method3

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- simdatac[[i]]

  data_sim_list <- lapply(unique(data_sim$id), function(id) {
    data_sim[data_sim$id == id, ]
  })

  ci <-calc_ci_95three(data_sim_list)

  out<-data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = cores[1] - 1)

resultc3<-Reduce("+", cover_mc)

print(resultc3$cover_p/2000)
print(resultc3$ci_width/2000)


#######################

#################
#parameterd+method1


cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- simdatad[[i]]

  roc<-roc(Exposed~wvELISA,data=data_sim)
  # calculate 95% confidence interval for AUC here
  ci<-ci.auc(roc, conf.level=0.95, method=c( "bootstrap"), boot.n = 1000)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.7494095 & 0.7494095< ci[3])),
    ci_width = (ci[3]-ci[1])

  )

}, mc.cores = cores[1] - 1)


resultd1<-Reduce("+", cover_mc)

print(resultd1$cover_p/2000)
print(resultd1$ci_width/2000)

#########################parameter4, method1(our code)


cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- simdatad[[i]]


  # calculate 95% confidence interval for AUC here
  ci <-calc_ci_95one(data_sim)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.7494095 & 0.7494095< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)

resultd11<-Reduce("+", cover_mc)

print(resultd11$cover_p/2000)
print(resultd11$ci_width/2000)

######################
###parameterd+method2


cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- simdatad[[i]]

  data_sim_list <- lapply(unique(data_sim$id), function(id) {
    data_sim[data_sim$id == id, ]
  })

  # calculate 95% confidence interval here

  ci <-calc_ci_95two(data_sim_list)

  out<-data.frame(
    cover_p = as.numeric((ci[1] <0.7494095 & 0.7494095< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)


resultd2<-Reduce("+", cover_mc)

# results divided by total simulation times will be the final results.

print(resultd2$cover_p/2000)
print(resultd2$ci_width/2000)


#############################
##parameterd+method3

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- simdatad[[i]]

  data_sim_list <- lapply(unique(data_sim$id), function(id) {
    data_sim[data_sim$id == id, ]
  })

  ci <-calc_ci_95three(data_sim_list)

  out<-data.frame(
    cover_p = as.numeric((ci[1] <0.7494095 & 0.7494095< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = cores[1] - 1)

resultd3<-Reduce("+", cover_mc)

print(resultd3$cover_p/2000)
print(resultd3$ci_width/2000)


##################

#simulation parametere+method1


cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- simdatae[[i]]

  roc<-roc(Exposed~wvELISA,data=data_sim)


  ci<-ci.auc(roc, conf.level=0.95, method=c( "bootstrap"), boot.n = 1000)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[3])),
    ci_width = (ci[3]-ci[1])

  )

}, mc.cores = 23)


resulte1<-Reduce("+", cover_mc)

print(resulte1$cover_p/2000)
print(resulte1$ci_width/2000)

#####################parametere, method1(our code)


cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- simdatae[[i]]

  # calculate 95% confidence interval for AUC here
  ci <-calc_ci_95one(data_sim)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)


resulte11<-Reduce("+", cover_mc)

print(resulte11$cover_p/2000)
print(resulte11$ci_width/2000)


##########################

#####simulation parameter5 +method 2

cover_mc <- mclapply(1:2000, function(i) {


  data_sim <- simdatae[[i]]

  data_sim_list <- lapply(unique(data_sim$id), function(id) {
    data_sim[data_sim$id == id, ]
  })

  ci <-calc_ci_95two(data_sim_list)

  out<-data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)

resulte2<-Reduce("+", cover_mc)

print(resulte2$cover_p/2000)
print(resulte2$ci_width/2000)



#########################

# parametere+method3

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- simdatae[[i]]

  data_sim_list <- lapply(unique(data_sim$id), function(id) {
    data_sim[data_sim$id == id, ]
  })

  ci <-calc_ci_95three(data_sim_list)

  out<-data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)

resulte3<-Reduce("+", cover_mc)

print(resulte3$cover_p/2000)
print(resulte3$ci_width/2000)


###################################################################
#####parameterf+method1
cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- simdataf[[i]]

  roc<-roc(Exposed~wvELISA,data=data_sim)

  ci<-ci.auc(roc, conf.level=0.95, method=c( "bootstrap"), boot.n = 1000)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.7494095 & 0.7494095< ci[3])),
    ci_width = (ci[3]-ci[1])

  )

}, mc.cores = 23)


resultf1<-Reduce("+", cover_mc)

print(resultf1$cover_p/2000)
print(resultf1$ci_width/2000)

#####################parameterf, method1(our code)

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- simdataf[[i]]


  # calculate 95% confidence interval for AUC here
  ci <-calc_ci_95one(data_sim)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.7494095 & 0.7494095< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)

resultf11<-Reduce("+", cover_mc)
print(resultf11$cover_p/2000)
print(resultf11$ci_width/2000)

#####parameterf +method2

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- simdataf[[i]]

  data_sim_list <- lapply(unique(data_sim$id), function(id) {
    data_sim[data_sim$id == id, ]
  })


  ci <-calc_ci_95two(data_sim_list)

  out<-data.frame(
    cover_p = as.numeric((ci[1] <0.7494095 & 0.7494095< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)


resultf2<-Reduce("+", cover_mc)


print(resultf2$cover_p/2000)
print(resultf2$ci_width/2000)



#####parameterf +method3


cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- simdataf[[i]]

  data_sim_list <- lapply(unique(data_sim$id), function(id) {
    data_sim[data_sim$id == id, ]
  })

  ci <-calc_ci_95three(data_sim_list)

  out<-data.frame(
    cover_p = as.numeric((ci[1] <0.7494095 & 0.7494095< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)


resultf3<-Reduce("+", cover_mc)

print(resultf3$cover_p/2000)

print(resultf3$ci_width/2000)

