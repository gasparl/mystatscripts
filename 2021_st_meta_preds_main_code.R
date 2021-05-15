

########################
#  load the packages   #
#                      #
#                      #
########################

library(plyr)
library(weights)
library(tidyverse)
library(haven)
options(scipen=999)
library(metafor)
library(schoolmath)
library(MBESS)
library(bayestestR)
library(BayesFactor)
library(pROC)
library(neatStats)
library(esc)

# Set working directory
setwd(path_neat())

# set the functions
source("2021_st_meta_preds_functions.R")


########################
#  Read in the data    #
#                      #
########################

##### USING cit_meta_data_trial_level.Rda

load("cit_meta_data_trial_level.Rda")
names(cit_meta_data_trial_level) = c('study', 'cond','multiple_single', 'id','block','dataset','trial','type','corr','rt','stim', 'isi','age','gender')
Data_joined = cit_meta_data_trial_level

dsets = unique(Data_joined$dataset)

# now we loop through the Data
set.seed(100)
metdat <- tibble()
count <- 0

for (i in dsets[order(nchar(dsets), dsets)]) {
    # i = "dataset 12"
  dat_i <- filter(Data_joined, dataset == i) # select the current data set

  study_i <- dat_i$study[1]
  cat("\n------------ started ", i, ": ", study_i, fill = T)

  datnum = as.numeric(strsplit(i, split = " ", fixed = TRUE)[[1]][2])

  #prep the data

  dat_i_prep <-
    dat_prep(
      dat_i$id,
      dat_i$gender,
      dat_i$age,
      dat_i$stim,
      dat_i$trial,
      dat_i$cond,
      dat_i$rt,
      dat_i$corr,
      dat_i$type,
      dat_i$multiple_single,
      dat_i$study,
      datnum
    )

  if (is.null(dat_i_prep$overall_acc_itarget)) {
    dat_i_prep = excl_neat(dat_i_prep,
                           filt = (
                             overall_acc_target > 0.5 &
                             overall_acc_allmain > 0.75
                           ))
  } else {
    dat_i_prep = excl_neat(dat_i_prep,
                           filt = (
                             overall_acc_target > 0.5 &
                             overall_acc_allmain > 0.75 &
                             (is.na(overall_acc_itarget) |
                              overall_acc_itarget > 0.5) &
                             (is.na(overall_acc_nontarget) |
                              overall_acc_nontarget > 0.5)
                           ))
  }

  dat_i_prep$rt_mean_diff = dat_i_prep$rt_mean_probe - dat_i_prep$rt_mean_irrelevant
  # Get the cohens d and stuff
  eff_data <- effectsize_data(dat_i_prep)

  met_dat_i = eff_data
  met_dat_i$dataset = datnum

  if (count > 0) {
      metdat <- rbind(metdat, met_dat_i)
      all_predicts = plyr::rbind.fill(all_predicts, dat_i_prep)
  } else
  {
      metdat <- met_dat_i
      all_predicts = dat_i_prep
  }
  count = count + 1
  cat("finished", i, ": ", study_i, fill = T)
}

# saveRDS(all_predicts, "standard_predictors_meta.rds")
# saveRDS(metdat, "standard_aucs_meta.rds")


