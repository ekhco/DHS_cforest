# ------------------------------------------------------------
# A random forest prediction using Zimbabwe Standard DHS 2015
# Zimbabwe 2015 selected because it has IR, MR, and AR
# survhiv71_flattened.dta
#
# Eric Chow, 2018-05-14
# ------------------------------------------------------------

  # try(install.packages("tidyverse"))
  # try(install.packages("foreign"))
  # try(install.packages("caret"))
  # try(install.packages("stringr"))
  # try(install.packages("randomForest"))
  # try(install.packages("ggplot2"))
  # try(install.packages("Boruta"))
  # try(install.packages("pROC")  )
  # try(install.packages("e1071"))
  # try(install.packages("haven"))

rm(list=ls())
gc()
# remove.packages("haven")
# devtools::install_version("haven", version = "1.1.0", repos = "http://cran.us.r-project.org")
# library(plyr)
# library(devtools)   # install.packages("devtools")
# library(reprtree)   # install_github('araastat/reprtree')
# library(glmnet)     # Library for glmnet ie: LASSO
# library(doParallel) # Library for parallel ops

library(tidyverse)
library(haven)
library(foreign)
library(caret)
library(stringr)
library(randomForest)
library(ggplot2)
library(Boruta)     # feature selection heuristic
library(pROC)       # for the AUC
library(e1071)
library(plyr)
# setwd("D:/EricChow/DHS_ranforest")
setwd("~/QSU/DHS_boruta")
source("0_fn.R")        # load functions
source("10_boruta.R")   # load main boruta main function

# get list of surveys
survey_dir <- "~/DHS_live_abstract/DHS_live_abstract/"
srvRDS_dir <- "~/DHS_live_abstract/DHS_RDS/"
mtadat_dir <- "~/DHS_live_abstract/meta_data/"
survey_files <- list.files(srvRDS_dir)
surveys <- substr(survey_files,1,4) # the survey country/wave ie: zw61

# get arguments for which countries to do
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {frange <- 1:length(surveys)} # do all the surveys
if (length(args)==1) {frange <- as.numeric(args[1])}  # do one survey
if (length(args)> 1) {frange <- as.numeric(args[1]):as.numeric(args[2])} # do only this range

# ------------------------------------------------------------------------------
# Run this if you want to do a one-off survey run (go back to 10_boruta)
# ------------------------------------------------------------------------------

if (1==0) {


                  # for one-off run
                  surveys <- c("mw7a", "et71", "zm63", "ls72", "ao71")
                  file <- this_survey <- surveys[4]
                  survey_filepath <- str_c(srvRDS_dir, file, ".rds", sep="")  # 27 works
                  mtadat_filepath <- str_c(mtadat_dir, file, "_metadat.dta", sep="")
                  sex = 1
                  w_neg = 1 ; w_pos = 4


                  frange = 7

}

# ------------------------------------------------------------------------------
# FOR THE ALL THE MALES/FEMALES and for each survey, run boruta ...
# ------------------------------------------------------------------------------

cat(date()); all_results <- NULL
for (file in surveys[frange]) {
    # get the survey filepath and it's corresponding metadata filepath
    # survey_filepath <- str_c(survey_dir, survey, "_flattened.dta", sep="")
    survey_filepath <- str_c(srvRDS_dir, file, ".rds", sep="")
    mtadat_filepath <- str_c(mtadat_dir, file, "_metadat.dta", sep="")

    # print it out
    message("\nANALYZING ", file, " ------------------------------------------------------------")

    # try BORUTA
    this_result_m <- NULL
    this_result_f <- NULL

    # set class weights
    w_neg = 1 ; w_pos = 3

    # 1 male, 2 female
    try(   this_result_m <- cbind(do_boruta(survey_filepath, mtadat_filepath, sex=1, this_survey = file, skip_boruta = FALSE, w_neg=w_neg, w_pos=w_pos, seed = 314),data.frame(SURVEY = file)) ) # MALES
    try(   this_result_f <- cbind(do_boruta(survey_filepath, mtadat_filepath, sex=2, this_survey = file, skip_boruta = FALSE, w_neg=w_neg, w_pos=w_pos, seed = 314),data.frame(SURVEY = file)) ) # FEMALES
    cat(date()) # output the date finished
    all_results <- rbind(all_results, this_result_m)
    all_results <- rbind(all_results, this_result_f)

    # write out the prediction/validation results for this M/F survey
    # write.dta(all_results, str_c("output/MF_validation_", file, ".dta", sep=""))
}
all_results_AUC <- all_results


# write to a file -----------------------
write.dta(all_results, str_c("output/MF_validation_", min(frange), "_", max(frange), "_w", w_neg, "-", w_pos, ".dta"))

# show all results
head(all_results)
nrow(all_results)
date()
