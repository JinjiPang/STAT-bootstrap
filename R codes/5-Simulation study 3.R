####Simulation Study 3

Exposed = c(rep(c(0,0,0,1,1,1), 100))

set.seed(2022)

sim3data1 <- lapply(1:2000, function(i) {

  data_sim <- data.frame(Exposed = Exposed,
                         wvELISA = -0.05636 + 1.38728*Exposed + rep(rnorm(100, 0, sd = 0.4879), each = 6) + rnorm(600, 0, sd = 0.5419),
                         id = rep(1:100, each = 6))

})

sim3data2<- lapply(1:2000, function(i) {

  data_sim <- data.frame(Exposed = Exposed,
                         wvELISA = -0.05636 + 0.69364*Exposed + rep(rnorm(100, 0, sd = 0.4879), each = 6) + rnorm(600, 0, sd = 0.5419),
                         id = rep(1:100, each = 6))

})

sim3data3 <- lapply(1:2000, function(i) {

  data_sim <- data.frame(Exposed = Exposed,
                         wvELISA = -0.05636 + 1.38728*Exposed + rep(rnorm(100, 0, sd = 0.1), each = 6) + rnorm(600, 0, sd = 0.7222),
                         id = rep(1:100, each = 6))

})

sim3data4 <- lapply(1:2000, function(i) {

  data_sim <- data.frame(Exposed = Exposed,
                         wvELISA = -0.05636 + 0.69364*Exposed + rep(rnorm(100, 0, sd = 0.1), each = 6) + rnorm(600, 0, sd = 0.7222),
                         id = rep(1:100, each = 6))

})

sim3data5 <- lapply(1:2000, function(i) {

  data_sim <- data.frame(Exposed = Exposed,
                         wvELISA = -0.05636 + 1.38728*Exposed + rep(rnorm(100, 0, sd = 0.7222), each = 6) + rnorm(600, 0, sd = 0.1),
                         id = rep(1:100, each = 6))

})

sim3data6 <- lapply(1:2000, function(i) {

  data_sim <- data.frame(Exposed = Exposed,
                         wvELISA = -0.05636 + 0.69364*Exposed + rep(rnorm(100, 0, sd = 0.7222), each = 6) + rnorm(600, 0, sd = 0.1),
                         id = rep(1:100, each = 6))

})


##########################################################
cores=detectCores()

# because we are using `parallel`  processing to minimize the running time, 
# cores[1]=24
# here we used maximum (cores -1)=23 to do the simulation
# change mc.cores = cores[1]-1 to accommodate your working machine. 
##simulation parameter 1 +method1(pROC package)

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim3data1[[i]]

  roc<-roc(Exposed~wvELISA,data=data_sim)
  # calculate 95% confidence interval for AUC here
  ci<-ci.auc(roc, conf.level=0.95, method=c( "bootstrap"), boot.n = 1000)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[3])),
    ci_width = (ci[3]-ci[1])

  )

}, mc.cores = 23)

resultda1<-Reduce("+", cover_mc)

print(resultda1$cover_p/2000)
print(resultda1$ci_width/2000)
############

# > print(resultda1$cover_p/2000)
# [1] 0.9535
# > print(resultda1$ci_width/2000)
# [1] 0.04437621


## simulation parameter 1 +method1(our code)

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim3data1[[i]]

  # calculate 95% confidence interval for AUC here
  ci <-calc_ci_95one(data_sim)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)

resultda11<-Reduce("+", cover_mc)

print(resultda11$cover_p/2000)
print(resultda11$ci_width/2000)

# > print(resultda11$cover_p/2000)
# [1] 0.9525
# > print(resultda11$ci_width/2000)
# [1] 0.04445861

##simulation parameter1 +method 2

cover_mc <- mclapply(1:2000, function(i) {


  data_sim <- sim3data1[[i]]

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

resultda2<-Reduce("+", cover_mc)

print(resultda2$cover_p/2000)

print(resultda2$ci_width/2000)


# > print(resultda2$cover_p/2000)
# [1] 0.9475
# > print(resultda2$ci_width/2000)
# [1] 0.04388297



##parameter1+method3

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim3data1[[i]]

  data_sim_list <- lapply(unique(data_sim$id), function(id) {
    data_sim[data_sim$id == id, ]
  })

  ci <-calc_ci_95three(data_sim_list)

  out<-data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = cores[1] - 1)

resultda3<-Reduce("+", cover_mc)

print(resultda3$cover_p/2000)
print(resultda3$ci_width/2000)


# > print(resultda3$cover_p/2000)
# [1] 0.9905
# > print(resultda3$ci_width/2000)
# [1] 0.05984367


##########################################################

##parameter 2+method1(pROC code)
cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim3data2[[i]]

  roc<-roc(Exposed~wvELISA,data=data_sim)

  ci<-ci.auc(roc, conf.level=0.95, method=c( "bootstrap"), boot.n = 1000)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.7494095 & 0.7494095< ci[3])),
    ci_width = (ci[3]-ci[1])

  )

}, mc.cores = 23)


resultda21<-Reduce("+", cover_mc)

print(resultda21$cover_p/2000)
print(resultda21$ci_width/2000)


# > print(resultda21$cover_p/2000)
# [1] 0.9735
# > print(resultda21$ci_width/2000)
# [1] 0.07666161

## simulation parameter 2+method 1(our code)

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim3data2[[i]]

  # calculate 95% confidence interval for AUC here
  ci <-calc_ci_95one(data_sim)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.7494095 & 0.7494095< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)

resultda211<-Reduce("+", cover_mc)

print(resultda211$cover_p/2000)
print(resultda211$ci_width/2000)

# > print(resultda211$cover_p/2000)
# [1] 0.974
# > print(resultda211$ci_width/2000)
# [1] 0.07672546


##simulation parameter 2 +method2

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim3data2[[i]]

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

resultda22<-Reduce("+", cover_mc)

# results divided by total simulation times will be the final results.

print(resultda22$cover_p/2000)
print(resultda22$ci_width/2000)

# > print(resultda22$cover_p/2000)
# [1] 0.937
# > print(resultda22$ci_width/2000)
# [1] 0.06431699

##simulation parameter 2 + method3

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim3data2[[i]]

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


resultda23<-Reduce("+", cover_mc)


print(resultda23$cover_p/2000)

print(resultda23$ci_width/2000)


# > print(resultda23$cover_p/2000)
# [1] 0.996
# > print(resultda23$ci_width/2000)
# [1] 0.09663696



##########################################################

##simulation parameter 3 +method1(pROC)

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim3data3[[i]]

  roc<-roc(Exposed~wvELISA,data=data_sim)
  # calculate 95% confidence interval for AUC here
  ci<-ci.auc(roc, conf.level=0.95, method=c( "bootstrap"), boot.n = 1000)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[3])),
    ci_width = (ci[3]-ci[1])

  )

}, mc.cores = 23)


resultda31<-Reduce("+", cover_mc)

print(resultda31$cover_p/2000)
print(resultda31$ci_width/2000)


# > print(resultda31$cover_p/2000)
# [1] 0.949
# > print(resultda31$ci_width/2000)
# [1] 0.04457284

## parameter3, method1(our code)

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim3data3[[i]]

  # calculate 95% confidence interval for AUC here
  ci <-calc_ci_95one(data_sim)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)


resultda311<-Reduce("+", cover_mc)

print(resultda311$cover_p/2000)
print(resultda311$ci_width/2000)



# > print(resultda311$cover_p/2000)
# [1] 0.949
# > print(resultda311$ci_width/2000)
# [1] 0.04466207

##parameter3 +method2

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim3data3[[i]]

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


resultda32<-Reduce("+", cover_mc)

print(resultda32$cover_p/2000)
print(resultda32$ci_width/2000)

# > print(resultda32$cover_p/2000)
# [1] 0.94
# > print(resultda32$ci_width/2000)
# [1] 0.04397448


##parameter3 + method3

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim3data3[[i]]

  data_sim_list <- lapply(unique(data_sim$id), function(id) {
    data_sim[data_sim$id == id, ]
  })

  ci <-calc_ci_95three(data_sim_list)

  out<-data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)

resultda33<-Reduce("+", cover_mc)

print(resultda33$cover_p/2000)
print(resultda33$ci_width/2000)

# > print(resultda33$cover_p/2000)
# [1] 0.991
# > print(resultda33$ci_width/2000)
# [1] 0.06005985



##########################################################

##parameter4 +method1(pROC code)

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim3data4[[i]]

  roc<-roc(Exposed~wvELISA,data=data_sim)
  # calculate 95% confidence interval for AUC here
  ci<-ci.auc(roc, conf.level=0.95, method=c( "bootstrap"), boot.n = 1000)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.7494095 & 0.7494095< ci[3])),
    ci_width = (ci[3]-ci[1])

  )

}, mc.cores = 23)

resultda41<-Reduce("+", cover_mc)

print(resultda41$cover_p/2000)
print(resultda41$ci_width/2000)

# > print(resultda41$cover_p/2000)
# [1] 0.955
# > print(resultda41$ci_width/2000)
# [1] 0.07667516


##parameter4, method1(our code)

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim3data4[[i]]


  # calculate 95% confidence interval for AUC here
  ci <-calc_ci_95one(data_sim)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.7494095 & 0.7494095< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)

resultda411<-Reduce("+", cover_mc)

print(resultda411$cover_p/2000)
print(resultda411$ci_width/2000)

# > print(resultda411$cover_p/2000)
# [1] 0.951
# > print(resultda411$ci_width/2000)
# [1] 0.07675031


##parameter 4+method2

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim3data4[[i]]

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


resultda42<-Reduce("+", cover_mc)

print(resultda42$cover_p/2000)
print(resultda42$ci_width/2000)


# > print(resultda42$cover_p/2000)
# [1] 0.9525
# > print(resultda42$ci_width/2000)
# [1] 0.07556902


##parameter4 +method3

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim3data4[[i]]

  data_sim_list <- lapply(unique(data_sim$id), function(id) {
    data_sim[data_sim$id == id, ]
  })

  ci <-calc_ci_95three(data_sim_list)

  out<-data.frame(
    cover_p = as.numeric((ci[1] <0.7494095 & 0.7494095< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)

resultda43<-Reduce("+", cover_mc)

print(resultda43$cover_p/2000)
print(resultda43$ci_width/2000)



# > print(resultda43$cover_p/2000)
# [1] 0.992
# > print(resultda43$ci_width/2000)
# [1] 0.1031868


##########################################################

##simulation parameter5 +method1(pROC)


cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim3data5[[i]]

  roc<-roc(Exposed~wvELISA,data=data_sim)


  ci<-ci.auc(roc, conf.level=0.95, method=c( "bootstrap"), boot.n = 1000)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[3])),
    ci_width = (ci[3]-ci[1])

  )

}, mc.cores = 23)


resultda51<-Reduce("+", cover_mc)

print(resultda51$cover_p/2000)
print(resultda51$ci_width/2000)

# > print(resultda51$cover_p/2000)
# [1] 0.8295
# > print(resultda51$ci_width/2000)
# [1] 0.04432985

##parameter5, method1(our code)

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim3data5[[i]]

  # calculate 95% confidence interval for AUC here
  ci <-calc_ci_95one(data_sim)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)


resultda511<-Reduce("+", cover_mc)

print(resultda511$cover_p/2000)
print(resultda511$ci_width/2000)


# > print(resultda511$cover_p/2000)
# [1] 0.8335
# > print(resultda511$ci_width/2000)
# [1] 0.04436257


##simulation parameter5 +method 2

cover_mc <- mclapply(1:2000, function(i) {


  data_sim <- sim3data5[[i]]

  data_sim_list <- lapply(unique(data_sim$id), function(id) {
    data_sim[data_sim$id == id, ]
  })

  ci <-calc_ci_95two(data_sim_list)

  out<-data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)

resultda52<-Reduce("+", cover_mc)

print(resultda52$cover_p/2000)
print(resultda52$ci_width/2000)

# > print(resultda52$cover_p/2000)
# [1] 0.9345
# > print(resultda52$ci_width/2000)
# [1] 0.05907415


##parameter 5 +method3

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim3data5[[i]]

  data_sim_list <- lapply(unique(data_sim$id), function(id) {
    data_sim[data_sim$id == id, ]
  })

  ci <-calc_ci_95three(data_sim_list)

  out<-data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)

resultda53<-Reduce("+", cover_mc)

print(resultda53$cover_p/2000)
print(resultda53$ci_width/2000)

# > print(resultda53$cover_p/2000)
# [1] 0.971
# > print(resultda53$ci_width/2000)
# [1] 0.06990348

##########################################################

##parameter6 +method1(pROC)
cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim3data6[[i]]

  roc<-roc(Exposed~wvELISA,data=data_sim)

  ci<-ci.auc(roc, conf.level=0.95, method=c( "bootstrap"), boot.n = 1000)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.7494095 & 0.7494095< ci[3])),
    ci_width = (ci[3]-ci[1])

  )

}, mc.cores = 23)


resultda61<-Reduce("+", cover_mc)

print(resultda61$cover_p/2000)
print(resultda61$ci_width/2000)


# > print(resultda61$cover_p/2000)
# [1] 0.9735
# > print(resultda61$ci_width/2000)
# [1] 0.07658191

##parameter6, method1(our code)

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim3data6[[i]]


  # calculate 95% confidence interval for AUC here
  ci <-calc_ci_95one(data_sim)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.7494095 & 0.7494095< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)

resultda611<-Reduce("+", cover_mc)
print(resultda611$cover_p/2000)
print(resultda611$ci_width/2000)


# > print(resultda611$cover_p/2000)
# [1] 0.974
# > print(resultda611$ci_width/2000)
# [1] 0.07665263

##parameter6 +method2

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim3data6[[i]]

  data_sim_list <- lapply(unique(data_sim$id), function(id) {
    data_sim[data_sim$id == id, ]
  })


  ci <-calc_ci_95two(data_sim_list)

  out<-data.frame(
    cover_p = as.numeric((ci[1] <0.7494095 & 0.7494095< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)


resultda62<-Reduce("+", cover_mc)


print(resultda62$cover_p/2000)
print(resultda62$ci_width/2000)

# > print(resultda62$cover_p/2000)
# [1] 0.937
# > print(resultda62$ci_width/2000)
# [1] 0.06324439

##parameter6 +method3

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim3data6[[i]]

  data_sim_list <- lapply(unique(data_sim$id), function(id) {
    data_sim[data_sim$id == id, ]
  })

  ci <-calc_ci_95three(data_sim_list)

  out<-data.frame(
    cover_p = as.numeric((ci[1] <0.7494095 & 0.7494095< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)


resultda63<-Reduce("+", cover_mc)

print(resultda63$cover_p/2000)

print(resultda63$ci_width/2000)


# > print(resultda63$cover_p/2000)
# [1] 0.9925
# > print(resultda63$ci_width/2000)
# [1] 0.09610061

