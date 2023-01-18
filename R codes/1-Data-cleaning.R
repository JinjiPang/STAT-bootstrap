

## Load packages
library(tidyverse)
library(boot)
library(lme4)
library(pROC)
library(parallel)
library(here)

######################################################################

## 1.Read in data

##please set STAT-bootstrap as working directory if using 
##dfroc<-read.csv("../data/ROC.csv")

dfroc<-read.csv(here("data","ROC.csv"))
dfroc$Pig.ID<-as.character(dfroc$Pig.ID)
dfroc$Exposed<-as.factor(dfroc$Exposed)

## 2.Remove 8 NA wvELISA observtions
dfrocnew<-dfroc%>%filter(!is.na(wvELISA))
pig_id <- unique(dfrocnew$Pig.ID)
length(pig_id)

######################################################################

## 3.Fit Linear Mixed Effect Model

elisa_mixed = lmer(wvELISA ~ Exposed + (1 | Pig.ID), data = dfrocnew)

summary(elisa_mixed)

# Random effects:
#   Groups   Name        Variance Std.Dev.
# Pig.ID   (Intercept) 0.2381   0.4879
# Residual             0.2936   0.5419
# Number of obs: 364, groups:  Pig.ID, 72
#
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept) -0.05636    0.08185  -0.689
# ExposedPOS   1.38728    0.08833  15.705

######################################################################

## Get the parameters for positive observations and negative observations based on 
## the linear mixed effect model's results

intercept <- -0.05636
fixed_coef <- 1.38728

sigma_p <- 0.4879
sigma_e <- 0.5419

random_effect <- rnorm(100, 0, sd = sigma_p)
random_effect_pigs <- rep(random_effect, each = 6)
eps <- rnorm(600, 0, sd = sigma_e)
exposed <- c(rep(0, 6*40), rep(c(0, 0, rep(1, 4)), 60))

y <- intercept + fixed_coef * exposed + random_effect_pigs + eps

hist(y)
sigma_y <- sqrt(sigma_p^2 + sigma_e^2)
# sigma_y=0.729179

######################################################################

## Draw the plot for the two normal distributions
# for negative observation, the distribution is rnorm(1, mean = intercept, sd = sigma_y)
# for positive observation, the distribution is rnorm(1, mean = intercept + fixed_coef, sd = sigma_y) 
mean_n <- -0.05636
sd_n <- 0.729179
mean_p<-1.33092
sd_p<-0.729179

curve(dnorm(x, mean=-0.05636, sd=0.729179), from = -6, to = 6)
curve(dnorm(x, mean=1.33092, sd=0.729179), add = TRUE, col = 2)


######################################################################

##parameters for two normal distributions, _n means negative population, _p means positive population
mean_n <- -0.05636
sd_n <- 0.729179
mean_p<-1.33092
sd_p<-0.729179

## get ROC data 
compute_roc_statistics <- function(threshold, mean_n, sd_n, mean_p, sd_p ) {
  
  TPR <- (1 - pnorm(threshold, mean = mean_p, sd = sd_p))
  FNR <- pnorm(threshold, mean = mean_p, sd = sd_p)
  
  FPR <- (1 - pnorm(threshold,  mean = mean_n, sd = sd_n))
  TNR <- pnorm(threshold, mean = mean_n, sd = sd_n)
  
  return( data.frame(TPR = TPR,FNR = FNR, FPR = FPR, TNR = TNR ) )
}


##input x, get y
auc_integrand <- function(x, df) {
  idx <- which.min(abs(df$FPR - x))[1]
  return(df$TPR[idx])
}

## allow input to be a vector
auc_integrand_vec <- Vectorize(auc_integrand, vectorize.args = 'x')

## compute AUC function
compute_auc <- function(mean_n, sd_n, mean_p, sd_p, subdivisions = 100L) {
  roc_result_list <- lapply(c(seq(-5, 15, by = 0.01)), compute_roc_statistics,
                            mean_n = mean_n, sd_n = sd_n, mean_p = mean_p, sd_p=sd_p)
  roc_result_df <- do.call(rbind, roc_result_list)
  
  integrate(auc_integrand_vec, lower = 0, upper = 1, df = roc_result_df, subdivisions = subdivisions)
}

true_AUC<-compute_auc(mean_n,sd_n,mean_p,sd_p,subdivisions = 100L)

print(true_AUC)

# true_AUC
# 0.910741 with absolute error < 7.2e-05

####draw the plot

roc_result_list <- lapply(c(seq(-5, 15, by = 0.01)), compute_roc_statistics,
                          mean_n = mean_n, sd_n = sd_n, mean_p = mean_p, sd_p=sd_p)
roc_result_df <- do.call(rbind, roc_result_list)

TrueAUC<-integrate(auc_integrand_vec, lower = 0, upper = 1, df = roc_result_df, subdivisions = 100L)

roc_result_df %>% ggplot() +
  geom_line(aes(x = FPR, y = TPR))





