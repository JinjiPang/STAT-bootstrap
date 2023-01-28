require(tidyverse)
require(boot)
require(pROC)


compute_auc1 <- function(data, indices, formula) {

  return(roc(formula, data=data[indices, ],quiet = TRUE)$auc)
}

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

##########

calc_ci <- function(data, id, formula, method, R = 1000) {

  if (method == "traditional") {
    result_boot <- boot(data = data, statistic = compute_auc1,
                        R = R, formula = formula)

  } else if (method == 'cluster') {

    subject_id <- unique(data[, id])

    data_sim_list <- lapply(subject_id, function(dd) {
      data[data[, id] == dd, ]
    })

    result_boot <- boot(data = subject_id, statistic = compute_auc2,
                        R = R, formula = formula, data_all = data_sim_list)

  } else if (method == 'hierarchical') {

    subject_id <- unique(data[, id])

    data_sim_list <- lapply(subject_id, function(dd) {
      data[data[, id] == dd, ]
    })

    result_boot <- boot(data = subject_id, statistic = compute_auc3,
                        R = R, formula = formula, data_all = data_sim_list)

  } else {
    stop("The specified method does not exist. Please use one of these: traditional, cluster, hierarchical.")
  }

  ci <- quantile(result_boot$t[,1], c(0.025, 0.975))
  return(ci)

}

# simulated data
Challenge_status = c(rep(0, 6*40), rep(c(0, rep(1, 5)), 60))


data_roc <- data.frame(Exposed = Challenge_status,
                       wvELISA = -0.05636 + 1.38728*Challenge_status + rep(rnorm(100, 0, sd = 0.4879), each = 6) + rnorm(600, 0, sd = 0.5419),
                       subject = rep(1:100, each = 6))


# compute 95% ...

# traditional
result<-calc_ci(data=data_roc, id="subject",formula = Exposed~ wvELISA,method = "cluster" ,R=1000 ); result
result<-calc_ci(data=data_roc, id="subject",formula = Exposed~ wvELISA,method = "hierarchical" ,R=1000 ); result
result<-calc_ci(data=data_roc, id="subject",formula = Exposed~ wvELISA,method = "traditional" ,R=1000 ); result
result<-calc_ci(data=data_roc, id="subject",formula = Exposed~ wvELISA,method = "xxx" ,R=1000 ); result
