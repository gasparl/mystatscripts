library('neatStats')
library('ggplot2')
library('lme4')

# Set working directory
setwd(path_neat())

metacit_dat = readRDS("meta_guilty_trials.rds")
metacit_dat = metacit_dat[metacit_dat$item_type != 'target',]

str(metacit_dat)

# decimate for fast testing
# metacit_dat = metacit_dat[seq(1, nrow(metacit_dat), 10), ]

# tests

mlm_fulmer = lmer(
  rt ~ item_type + trial_number +
     item_type:trial_number +
    (item_type | subject_id) + (item_type | dataset),
  data = metacit_dat
)

mlm_full = glmer(
  rt ~ item_type + trial_number +
    item_type:trial_number +
    (item_type | subject_id) + (item_type | dataset),
  data = metacit_dat,
  family = Gamma(link = "identity")
)

ggpubr::ggqqplot(resid(mlm_full), shape = 1)
ggpubr::ggdensity(resid(mlm_full))
lattice::xyplot(profile(mlm_full))
lattice::densityplot(profile(mlm_full))
# lattice::splom(profile(mlm_full))
plot(mlm_full, type = c("p", "smooth"))
plot(mlm_full, sqrt(abs(resid(.))) ~ fitted(.),type = c("p", "smooth"))

# summary(mlm_full)
# report::report(mlm_full)
# parameters::model_parameters(mlm_full)

mlm_xtrial = glmer(
  rt ~ item_type + trial_number +
    (item_type | subject_id) + (item_type | dataset),
  data = metacit_dat,
  family = Gamma(link = "identity")
)

aov_trials = anova(mlm_full, mlm_xtrial)

aov_trials

