## Nonparametric bootstrap methods for interval estimation of the area under the ROC curve for correlated diagnostic test data

This repository is a repository about the extended information for paper in Preventive Veterinary Medicine "Nonparametric bootstrap methods for interval estimation of the area under the ROC curve for correlated diagnostic test data". It mainly contains code for `calc_ci` function which can calculate 95% CI of AUC given a data, subject, and specified formula.


----------------------------------------

## Appendix

Here we described three nonparametric bootstrap methods, which are the traditional bootstrap, cluster bootstrap, and hierarchical bootstrap. By combining these three functions, we created a single function `calc_ci` which can calculate 95 % CI of AUC using three bootstrap methods.


---------------------------------------------------------------

Software and R packages versions are listed here for better reproducibility.

> sessionInfo()
R version 4.2.2 (2022-10-31)
Platform: aarch64-apple-darwin20 (64-bit)
Running under: macOS Monterey 12.5

Matrix products: default
LAPACK: /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] here_1.0.1      pROC_1.18.0     lme4_1.1-31     Matrix_1.5-1    boot_1.3-28     forcats_0.5.2  
 [7] stringr_1.4.1   dplyr_1.0.10    purrr_0.3.5     readr_2.1.3     tidyr_1.2.1     tibble_3.1.8   
[13] ggplot2_3.4.0   tidyverse_1.3.2


