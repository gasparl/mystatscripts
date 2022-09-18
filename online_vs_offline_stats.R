library('ggplot2')
library('data.table')
library('lme4')

oo_data = readRDS(neatStats::path_neat('online_vs_offline_data.rds'))

# tests

mlm_full = lmer(
  rt ~ item_type + trial_number +
    item_type:trial_number +
    (1 | subject_id) + (item_type | dataset),
  data = metacit_dat, REML = FALSE
)

# mlm_full5 = glmer(
#   rt ~ item_type + trial_number +
#     item_type:trial_number +
#     (1 | subject_id) + (item_type | dataset),
#   data = metacit_dat, family = Gamma(link = "identity")
# )

report::report(mlm_full)

###

mlm_xtrial = lmer(
  rt ~ item_type + trial_number +
    (item_type | subject_id) + (item_type | dataset),
  data = metacit_dat, REML = FALSE
)

aov_trials = anova(mlm_xtrial, mlm_full)
aov_trials

compp = performance::compare_performance(mlm_xtrial, mlm_full)
compp
plot(compp)
