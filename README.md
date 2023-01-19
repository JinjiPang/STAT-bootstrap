## Nonparametric bootstrap methods for interval estimation of the area under the ROC curve for correlated diagnostic test data

This repository is a repository about the extended information for paper in Preventive Veterinary Medicine "Nonparametric bootstrap methods for interval estimation of the area under the ROC curve for correlated diagnostic test data". It mainly contains code for real data analysis, three nonparametric bootstrap methods descriptions, and simulation studies.


----------------------------------------
## 1-Data-cleaning

The dataset (ROC.csv file) is from a published paper that utilizes different diagnostic assays to detect porcine192 parainfluenza virus type-1 antibody in swine serum (Welch et al., 2022). 

In this paper, we focused on the whole virus ELISA (wvELISA) assay method. We analyzed the data of wvELISA by fitting a linear mixed effect model and got the parameters for simulation studies. Based on the these parameters, the true AUC was also calculated.

## 2-Three methods functions

Here we described three nonparametric bootstrap methods, which are the traditional bootstrap, cluster bootstrap, and hierarchical bootstrap.


## 3-Simulation study 1

In the first simulation study, the data were simulated to examine a situation where the challenge
status of observations from the same subject may change during the test phase, miming the real
data. For each simulated dataset, 100 subjects were simulated, with 6 observations from each of
the subjects. Among these subjects, P1 (=40) subjects were assigned to the unchallenged group,
which means the challenge status of all the observations from these subjects was always
unchallenged. The other P2 (=60) subjects were assigned to the challenged group, and of the 6
observations for each subject, one observation had a challenge status that was unchallenged and
the remaining 5 observations were challenged.


## 4-Simulation study 2

The data were simulated in the second simulation study to examine a situation where the
challenge status of observations from the same subject does not change over the entire test phase.
For each simulated dataset, 100 subjects were simulated, with 6 observations from each of the
subjects. Among these subjects, P1 (=40) subjects were assigned to the unchallenged group, and
P2 (=60) subjects were assigned to the challenged group. The challenge status of the 6
observations was all unchallenged for the P1 subjects and was all challenged for the P2 subjects.


## 5-Simulation study 3

In the third simulation study, the data were simulated to examine a situation where half of
observations from the same subject were unchallenged, and the other half were challenged. For
each simulated dataset, 100 subjects were simulated, with 6 observations from each of the
subjects. The challenge status of the 6 observations of the same subject were half unchallenged
and half challenged.


## 6-wvELISA graph

This is the code to generate the ROC graph (Figure.1) for real data analysis.


## 7-Simulation study 3-window

This file contains codes for simulation analysis using Linux/Windows machines. Here we used simulation study 3 as an example. The major difference is to use `foreach` rather than `mcapply` for parallel processing.


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


