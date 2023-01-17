###
###add content based on a paper
library(MASS)

# We used a balanced repeated measures design with 200 subjects and 
# each subject had 4 repeated measures observations.

##simulate bi-normal distribution
sample_size <- 200                                       
sample_meanvector <- c(0, 0)                                   
sample_covariance_matrix <- matrix(c(4, 3.2, 3.2, 11.56),
                                   ncol = 2)

# create bivariate normal distribution
sample_distribution <- mvrnorm(n = sample_size,
                               mu = sample_meanvector, 
                               Sigma = sample_covariance_matrix)


Z1<-sample_distribution[,1]

A1<-sample_distribution[,2]

data_sim <- data.frame(Subject_ID =rep(c(1:200),each= 4),
                       Observation_ID = rep(c(1,2,3,4), 200),
                       T_status = rep(rbinom(200,1,0.4), each=4),
                      Yij = 10 + 3*rep(rnorm(200),each=4)- 4*T_status + rep(Z1,each=4)+ rep(A1,each=4) + rnorm(800, 0, sd = 2)
                      )


### 

roc1<-roc(T_status~Yij,data=data_sim)
# calculate 95% confidence interval for AUC here
ci<-ci.auc(roc1, conf.level=0.95, method=c( "bootstrap"), boot.n = 1000)


plot(roc1)
auc(roc1)
