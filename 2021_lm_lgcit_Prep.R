library('neatStats')
library('ggplot2')

# Set working directory
setwd(path_neat())

study1 = readRDS("lgcit_guilty_study1.rds")
study2 = readRDS("lgcit_guilty_study2.rds")
study1$study = 'study1'
study2$study = 'study2'
lgcit_dat = rbind(study1, study2)

lgcit_dat = lgcit_dat[lgcit_dat$valid_trial == 1 & lgcit_dat$stim_type %in% c('probe', 'irrelevant'),]
lgcit_dat$stim_type[lgcit_dat$stim_type == 'irrelevant'] = 'control'
lgcit_dat$block_number[lgcit_dat$block_number == 4] = 1
lgcit_dat$block_number[lgcit_dat$block_number == 6] = 2
lgcit_dat$block_number[lgcit_dat$block_number == 8] = 3
lgcit_dat$block_number[lgcit_dat$block_number == 10] = 4
lgcit_dat$trial_number = lgcit_dat$trial_number + (162 * (lgcit_dat$block_number-1))

saveRDS(lgcit_dat, "lgcit_guilty_both.rds")

str(lgcit_dat)

#### prep full

study1 = readRDS("lgcit_trials_study1.rds")
study2 = readRDS("lgcit_trials_study2.rds")
study1$study = 'study1'
study2$study = 'study2'
lgcit_dat2 = rbind(study1, study2)

lgcit_dat2 = lgcit_dat2[lgcit_dat2$valid_trial == 1 & lgcit_dat2$stim_type %in% c('probe', 'irrelevant', 'target'),]
lgcit_dat2$stim_type[lgcit_dat2$stim_type == 'irrelevant'] = 'control'
lgcit_dat2$block_number[lgcit_dat2$block_number == 4] = 1
lgcit_dat2$block_number[lgcit_dat2$block_number == 6] = 2
lgcit_dat2$block_number[lgcit_dat2$block_number == 8] = 3
lgcit_dat2$block_number[lgcit_dat2$block_number == 10] = 4

lgcit_dat2$subject_id = paste(lgcit_dat2$subject_id, lgcit_dat2$tested_lang, sep = '_')
lgcit_dat2$trial_number = lgcit_dat2$trial_number + (162 * (lgcit_dat2$block_number-1))

saveRDS(lgcit_dat2, "lgcit_trials_both.rds")

str(lgcit_dat2)
