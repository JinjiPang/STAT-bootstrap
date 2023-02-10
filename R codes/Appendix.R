require(tidyverse)
require(boot)
require(pROC)


##################################################################


## Cluster bootstrap method
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


## Hierarchical bootstrap method
compute_auc3 <- function(data, indices, formula, data_all) {
  # id from bootstrap sampling
  selected_id <- data[indices]
  #
  boot_data_list <- lapply(selected_id, function(id) {

    data_selected_subject <- data_all[[id]]
    # re-sample
    re_idx = sample(1:nrow(data_selected_subject), size = nrow(data_selected_subject),
                    replace = TRUE)

    data_selected_subject[re_idx, ]
  })

  # get all data with selected id
  boot_data <- do.call(rbind, boot_data_list)

  return(roc(formula, data=boot_data, quiet=TRUE)$auc)
}

##################################################################

## `calc_ci` is a function that can give confidence interval of AUC
## `data` is a matrix or data.frame containing the variables in the formula 
## `id` is the subject that brings subject-level random effect
## `formula` is a formula of the type response~predictor
## `method` the method to use, either “cluster” or “hierarchical”
## `resample` is the number of bootstrap replicates or permutations, default is 1000
## `conf.level` is the confidence level for AUC, default is 0.95

calc_ci <- function(data, id, formula, method="cluster", resample=1000, conf.level=0.95) {

    if (method == 'cluster') {

    subject_id <- unique(data[, id])

    data_sim_list <- lapply(subject_id, function(dd) {
      data[data[, id] == dd, ]
    })

    result_boot <- boot(data = subject_id, statistic = compute_auc2,
                        R = resample, formula = formula, data_all = data_sim_list)

  } else if (method == 'hierarchical') {

    subject_id <- unique(data[, id])

    data_sim_list <- lapply(subject_id, function(dd) {
      data[data[, id] == dd, ]
    })

    result_boot <- boot(data = subject_id, statistic = compute_auc3,
                        R = resample, formula = formula, data_all = data_sim_list)

  } else {
    stop("The specified method does not exist. Please use one of these: traditional, cluster, hierarchical.")
  }

  ci <- quantile(result_boot$t[,1], c((1-conf.level)/2,((1-conf.level)/2+conf.level)))
  return(
    
    result<-list(
      point_estimate= mean(result_boot$t[,1]),
      CI=ci,
      confidence_level=conf.level,
      resample=resample,
      method= method
  
    ))

}


# simulated data to demonstrate calc_ci function
Challenge_status = c(rep(0, 6*40), rep(c(0, rep(1, 5)), 60))

data_roc <- data.frame(Exposed = Challenge_status,
                       wvELISA = -0.05636 + 1.38728*Challenge_status + rep(rnorm(100, 0, sd = 0.4879), each = 6) + rnorm(600, 0, sd = 0.5419),
                       subject = rep(1:100, each = 6))


# compute 95% ...

# cluster(default) method

result<-calc_ci(data=data_roc, id="subject",formula = Exposed~ wvELISA); result

result<-calc_ci(data=data_roc, id="subject",formula = Exposed~ wvELISA, method = "cluster", resample=1000, conf.level=0.95 ); result

result<-calc_ci(data=data_roc, id="subject",formula = Exposed~ wvELISA, method = "cluster", resample=2000, conf.level=0.90 ); result


# hierarchical method

result<-calc_ci(data=data_roc, id="subject",formula = Exposed~ wvELISA, method = "hierarchical", resample =1000 ); result

result<-calc_ci(data=data_roc, id="subject",formula = Exposed~ wvELISA, method = "hierarchical", resample=2000, conf.level=0.90); result



