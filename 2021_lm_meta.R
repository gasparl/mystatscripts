library('neatStats')
library('ggplot2')
library('lme4')

# Set working directory
setwd(path_neat())

metacit_dat = readRDS("meta_guilty_trials.rds")
metacit_dat = metacit_dat[metacit_dat$item_type != 'target',]
metacit_dat$trial_number = metacit_dat$trial_number/100

str(metacit_dat)

# decimate for fast testing
# metacit_dat = metacit_dat[seq(1, nrow(metacit_dat), 10), ]

# tests

mlm_full = lmer(
  rt ~ item_type + trial_number +
    item_type:trial_number +
    (item_type | subject_id) + (item_type | dataset),
  data = metacit_dat, REML = FALSE
)

# mlm_full5 = glmer(
#   rt ~ item_type + trial_number +
#     item_type:trial_number +
#     (1 | subject_id) + (item_type | dataset),
#   data = metacit_dat, family = Gamma(link = "identity")
# )

summary(mlm_full3)

compp = performance::compare_performance(mlm_full3, mlm_full4, mlm_full5)
compp
plot(compp)

report::report(mlm_full)

###

mlm_xtrial = lmer(
  rt ~ item_type + trial_number +
    (item_type | subject_id) + (item_type | dataset),
  data = metacit_dat, REML = FALSE
)

aov_trials = anova(mlm_xtrial, mlm_full)
aov_trials
