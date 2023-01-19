library(boot)
library(pROC)
library(parallel)




Exposed = c(rep(0, 6*40), rep(c(0, rep(1, 5)), 60))

set.seed(2022)

sim1data1 <- lapply(1:2000, function(i) {

  data_sim <- data.frame(Exposed = Exposed,
                         wvELISA = -0.05636 + 1.38728*Exposed + rep(rnorm(100, 0, sd = 0.4879), each = 6) + rnorm(600, 0, sd = 0.5419),
                        id = rep(1:100, each = 6))

})

sim1data2 <- lapply(1:2000, function(i) {

  data_sim <- data.frame(Exposed = Exposed,
                         wvELISA = -0.05636 + 0.69364*Exposed + rep(rnorm(100, 0, sd = 0.4879), each = 6) + rnorm(600, 0, sd = 0.5419),
                         id = rep(1:100, each = 6))

})

sim1data3 <- lapply(1:2000, function(i) {

  data_sim <- data.frame(Exposed = Exposed,
                         wvELISA = -0.05636 + 1.38728*Exposed + rep(rnorm(100, 0, sd = 0.1), each = 6) + rnorm(600, 0, sd = 0.7222),
                         id = rep(1:100, each = 6))

})

sim1data4 <- lapply(1:2000, function(i) {

  data_sim <- data.frame(Exposed = Exposed,
                         wvELISA = -0.05636 + 0.69364*Exposed + rep(rnorm(100, 0, sd = 0.1), each = 6) + rnorm(600, 0, sd = 0.7222),
                         id = rep(1:100, each = 6))

})

sim1data5 <- lapply(1:2000, function(i) {

  data_sim <- data.frame(Exposed = Exposed,
                         wvELISA = -0.05636 + 1.38728*Exposed + rep(rnorm(100, 0, sd = 0.7222), each = 6) + rnorm(600, 0, sd = 0.1),
                         id = rep(1:100, each = 6))

})

sim1data6 <- lapply(1:2000, function(i) {

  data_sim <- data.frame(Exposed = Exposed,
                         wvELISA = -0.05636 + 0.69364*Exposed + rep(rnorm(100, 0, sd = 0.7222), each = 6) + rnorm(600, 0, sd = 0.1),
                         id = rep(1:100, each = 6))

})




############################################
cores=detectCores()

# because we are using `parallel`  processing to minimize the running time, 
# cores[1]=24
# here we used maximum (cores -1)=23 to do the simulation
# change mc.cores = cores[1]-1 to accommodate your working machine. 


##simulation parameter1+method1(pROC package)

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim1data1[[i]]

  roc<-roc(Exposed~wvELISA,data=data_sim)
  # calculate 95% confidence interval for AUC here
  ci<-ci.auc(roc, conf.level=0.95, method=c( "bootstrap"), boot.n = 1000)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[3])),
    ci_width = (ci[3]-ci[1])

  )

}, mc.cores = cores[1]-1)

result11<-Reduce("+", cover_mc)

print(result11$cover_p/2000)
print(result11$ci_width/2000)


##simulation parameter1+method1(our code)

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim1data1[[i]]


  # calculate 95% confidence interval for AUC here
  ci <-calc_ci_95one(data_sim)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)

result111<-Reduce("+", cover_mc)

print(result111$cover_p/2000)
print(result111$ci_width/2000)


##simulation parameter1 +method 2

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

}, mc.cores = 23)

result12<-Reduce("+", cover_mc)

print(result12$cover_p/2000)
print(result12$ci_width/2000)


##parameter1+method3

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

}, mc.cores = 23)

result13<-Reduce("+", cover_mc)

print(result13$cover_p/2000)
print(result13$ci_width/2000)


#################################################

##parameter2+method1(pROC code)

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim1data2[[i]]

  roc<-roc(Exposed~wvELISA,data=data_sim)

  ci<-ci.auc(roc, conf.level=0.95, method=c( "bootstrap"), boot.n = 1000)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.7494095 & 0.7494095< ci[3])),
    ci_width = (ci[3]-ci[1])

  )

}, mc.cores = 23)


result21<-Reduce("+", cover_mc)

print(result21$cover_p/2000)
print(result21$ci_width/2000)

##parameter2, method1(our code)


cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim1data2[[i]]


  # calculate 95% confidence interval for AUC here
  ci <-calc_ci_95one(data_sim)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.7494095 & 0.7494095< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)

result211<-Reduce("+", cover_mc)

print(result211$cover_p/2000)
print(result211$ci_width/2000)

##parameter2 +method2

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim1data2[[i]]

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


result22<-Reduce("+", cover_mc)

# results divided by total simulation times will be the final results.

print(result22$cover_p/2000)
print(result22$ci_width/2000)


##parameter2 +method3


cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim1data2[[i]]

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


result23<-Reduce("+", cover_mc)


print(result23$cover_p/2000)

print(result23$ci_width/2000)


############################################

##parameter3+method1(pROC code)


cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim1data3[[i]]

  roc<-roc(Exposed~wvELISA,data=data_sim)
  # calculate 95% confidence interval for AUC here
  ci<-ci.auc(roc, conf.level=0.95, method=c( "bootstrap"), boot.n = 1000)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[3])),
    ci_width = (ci[3]-ci[1])

  )

}, mc.cores = 23)


result31<-Reduce("+", cover_mc)

print(result31$cover_p/2000)
print(result31$ci_width/2000)


##parameter3, method1(our code)


cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim1data3[[i]]

  # calculate 95% confidence interval for AUC here
  ci <-calc_ci_95one(data_sim)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)


result311<-Reduce("+", cover_mc)

print(result311$cover_p/2000)
print(result311$ci_width/2000)


############################################
##parameter3+method2


cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim1data3[[i]]

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


result32<-Reduce("+", cover_mc)

print(result32$cover_p/2000)
print(result32$ci_width/2000)


##parameter3+method3

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim1data3[[i]]

  data_sim_list <- lapply(unique(data_sim$id), function(id) {
    data_sim[data_sim$id == id, ]
  })

  ci <-calc_ci_95three(data_sim_list)

  out<-data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = cores[1] - 1)

result33<-Reduce("+", cover_mc)

print(result33$cover_p/2000)
print(result33$ci_width/2000)



############################################

##parameter4+method1

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim1data4[[i]]

  roc<-roc(Exposed~wvELISA,data=data_sim)
  # calculate 95% confidence interval for AUC here
  ci<-ci.auc(roc, conf.level=0.95, method=c( "bootstrap"), boot.n = 1000)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.7494095 & 0.7494095< ci[3])),
    ci_width = (ci[3]-ci[1])

  )

}, mc.cores = cores[1] - 1)


result41<-Reduce("+", cover_mc)

print(result41$cover_p/2000)
print(result41$ci_width/2000)



##parameter4, method1(our code)

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim1data4[[i]]


  # calculate 95% confidence interval for AUC here
  ci <-calc_ci_95one(data_sim)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.7494095 & 0.7494095< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)

result411<-Reduce("+", cover_mc)

print(result411$cover_p/2000)
print(result411$ci_width/2000)


##parameter4+method2


cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim1data4[[i]]

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


result42<-Reduce("+", cover_mc)

# results divided by total simulation times will be the final results.

print(result42$cover_p/2000)
print(result42$ci_width/2000)


##parameter4+method3

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim1data4[[i]]

  data_sim_list <- lapply(unique(data_sim$id), function(id) {
    data_sim[data_sim$id == id, ]
  })

  ci <-calc_ci_95three(data_sim_list)

  out<-data.frame(
    cover_p = as.numeric((ci[1] <0.7494095 & 0.7494095< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = cores[1] - 1)

result43<-Reduce("+", cover_mc)

print(result43$cover_p/2000)
print(result43$ci_width/2000)



############################################

##simulation parameter5+method1(pROC code)


cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim1data5[[i]]

  roc<-roc(Exposed~wvELISA,data=data_sim)


  ci<-ci.auc(roc, conf.level=0.95, method=c( "bootstrap"), boot.n = 1000)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[3])),
    ci_width = (ci[3]-ci[1])

  )

}, mc.cores = 23)


result51<-Reduce("+", cover_mc)

print(result51$cover_p/2000)
print(result51$ci_width/2000)

##parameter5, method1(our code)

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim1data5[[i]]

  # calculate 95% confidence interval for AUC here
  ci <-calc_ci_95one(data_sim)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)


result511<-Reduce("+", cover_mc)

print(result511$cover_p/2000)
print(result511$ci_width/2000)

##simulation parameter5 +method 2

cover_mc <- mclapply(1:2000, function(i) {


  data_sim <- sim1data5[[i]]

  data_sim_list <- lapply(unique(data_sim$id), function(id) {
    data_sim[data_sim$id == id, ]
  })

  ci <-calc_ci_95two(data_sim_list)

  out<-data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)

result52<-Reduce("+", cover_mc)

print(result52$cover_p/2000)
print(result52$ci_width/2000)


##parameter5+method3

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim1data5[[i]]

  data_sim_list <- lapply(unique(data_sim$id), function(id) {
    data_sim[data_sim$id == id, ]
  })

  ci <-calc_ci_95three(data_sim_list)

  out<-data.frame(
    cover_p = as.numeric((ci[1] <0.910741 & 0.910741< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)

result53<-Reduce("+", cover_mc)

print(result53$cover_p/2000)
print(result53$ci_width/2000)


############################################

##parameter6+method1(pROC code)

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim1data6[[i]]

  roc<-roc(Exposed~wvELISA,data=data_sim)

  ci<-ci.auc(roc, conf.level=0.95, method=c( "bootstrap"), boot.n = 1000)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.7494095 & 0.7494095< ci[3])),
    ci_width = (ci[3]-ci[1])

  )

}, mc.cores = 23)


result61<-Reduce("+", cover_mc)

print(result61$cover_p/2000)
print(result61$ci_width/2000)

##parameter6, method1(our code)

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim1data6[[i]]


  # calculate 95% confidence interval for AUC here
  ci <-calc_ci_95one(data_sim)

  out <- data.frame(
    cover_p = as.numeric((ci[1] <0.7494095 & 0.7494095< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)

result611<-Reduce("+", cover_mc)

print(result611$cover_p/2000)
print(result611$ci_width/2000)


##parameter6 +method2

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim1data6[[i]]

  data_sim_list <- lapply(unique(data_sim$id), function(id) {
    data_sim[data_sim$id == id, ]
  })


  ci <-calc_ci_95two(data_sim_list)

  out<-data.frame(
    cover_p = as.numeric((ci[1] <0.7494095 & 0.7494095< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)


result62<-Reduce("+", cover_mc)


print(result62$cover_p/2000)
print(result62$ci_width/2000)

##parameter6 +method3

cover_mc <- mclapply(1:2000, function(i) {

  data_sim <- sim1data6[[i]]

  data_sim_list <- lapply(unique(data_sim$id), function(id) {
    data_sim[data_sim$id == id, ]
  })

  ci <-calc_ci_95three(data_sim_list)

  out<-data.frame(
    cover_p = as.numeric((ci[1] <0.7494095 & 0.7494095< ci[2])),
    ci_width = (ci[2]-ci[1])

  )

}, mc.cores = 23)


result63<-Reduce("+", cover_mc)

print(result63$cover_p/2000)

print(result63$ci_width/2000)

