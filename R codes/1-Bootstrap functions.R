library(boot)
library(tidyverse)
library(pROC)

##############################################################

### function for traditional bootstrap method
compute_auc1 <- function(data, indices, formula) {

  return(roc(formula, data=data[indices, ],quiet = TRUE)$auc)
}


calc_ci_95one=function(data){

  result_elisa1 <- boot(data=data, statistic=compute_auc1,
                        R=1000, formula=Exposed~ wvELISA)
  ci<-quantile(result_elisa1$t[,1], c(0.025, 0.975))

}

##############################################################

## function for cluster bootstrap method

compute_auc2 <- function(data, indices, formula, data_all) {
  # id from bootstrap sampling
  selected_id <- data[indices]
  #
  boot_data_list <- lapply(selected_id, function(id) {
    # data_all[data_all$id == id , ]
    data_all[[id]]
  })
  # get all data with selected id
  boot_data <- do.call(rbind, boot_data_list)
  # boot_data <- data_all[ data_all$id %in% selected_id, ]

  return(roc(formula, data=boot_data, quiet = TRUE)$auc)
}



calc_ci_95two=function(data){

  pig_id <- 1:length(data) # this data is list! unique(data$id)

  result_elisa2 <- boot(data=pig_id, statistic=compute_auc2,
                        R=1000, formula=Exposed~ wvELISA, data_all=data)

  ci<-quantile(result_elisa2$t[,1], c(0.025, 0.975))

}

##############################################################
## function for hierarchical bootstrap method

compute_auc3 <- function(data, indices, formula, data_all) {
  # id from bootstrap sampling
  selected_id <- data[indices]
  #
  boot_data_list <- lapply(selected_id, function(id) {

    data_selected_pig <- data_all[[id]]
    # re-sample
    re_idx = sample(1:nrow(data_selected_pig), size = nrow(data_selected_pig),
                    replace = TRUE)

    data_selected_pig[re_idx, ]
  })

  # get all data with selected id
  boot_data <- do.call(rbind, boot_data_list)

  return(roc(formula, data=boot_data, quiet=TRUE)$auc)
}


calc_ci_95three=function(data){

  pig_id <- 1:length(data)

  result_elisa3 <- boot(data=pig_id, statistic=compute_auc3,
                        R=1000, formula=Exposed~ wvELISA, data_all=data)

  ci<-quantile(result_elisa3$t[,1], c(0.025, 0.975))

}


