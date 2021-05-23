library('neatStats')
library('ggplot2')
library('lme4')

# Set working directory
setwd(path_neat())

lgcit_dat = readRDS("lgcit_guilty_both.rds")
lgcit_dat$trial_number = lgcit_dat$trial_number/100
str(lgcit_dat)

#length(unique(lgcit_dat$subject_id))
#nrow(lgcit_dat)/219

# lgcit_dat$block_number = ordered(lgcit_dat$block_number)

# tests

#mlm_fulmer = robustlmm::rlmer(
#mlm_fulmer = lmer(


mlm_full = lmer(
  rt_start ~ stim_type + block_number + trial_number +
    stim_type:block_number + stim_type:trial_number +
    (stim_type | subject_id),
  data = lgcit_dat, REML = FALSE
)

# mlm_full3 = glmer(
#   rt_start ~ stim_type + block_number + trial_number +
#     stim_type:block_number + stim_type:trial_number +
#     (stim_type | subject_id),
#   data = lgcit_dat,
#   family = Gamma(link = "identity")
# )

compp = performance::compare_performance(mlm_full, mlm_full9)
compp
plot(compp)

####
# performance::model_performance(mlm_full)
# performance::check_model(mlm_full)
# ggpubr::ggqqplot(resid(mlm_full), shape = 1)
# ggpubr::ggdensity(resid(mlm_full))
# lattice::xyplot(profile(mlm_fulmer))
# lattice::densityplot(profile(mlm_full))
# # lattice::splom(profile(mlm_full))
# plot(mlm_full, type = c("p", "smooth"))
# plot(mlm_full, sqrt(abs(resid(.))) ~ fitted(.),type = c("p", "smooth"))

# confint(mlm_full, method = 'Wald')
# summary(mlm_full)
# report::report(mlm_full)

mlm_xtrial = lmer(
  rt_start ~ stim_type + block_number + trial_number +
    stim_type:block_number +
    (stim_type | subject_id),
  data = lgcit_dat, REML = FALSE
)


aov_trials = anova(mlm_full, mlm_xtrial)

mlm_xblock = lmer(
  rt_start ~ stim_type + block_number + trial_number +
    stim_type:trial_number +
    (stim_type | subject_id),
  data = lgcit_dat, REML = FALSE
)

aov_blocks = anova(mlm_full, mlm_xblock)

aov_trials
aov_blocks


#
mlm_trial_only = lmer(
  rt_start ~ stim_type + trial_number +
    stim_type:trial_number +
    (stim_type | subject_id),
  data = lgcit_dat
)

## plots
model = mlm_full
#

fit_dat = fitted(model)

ee = effects::effect(c("trial_number","stim_type","block_number"),model)

theme_set(theme_bw())
ggplot(as.data.frame(ee),
       aes(trial_number,fit,colour=stim_type,fill=stim_type))+
  geom_line()+
  ## colour=NA suppresses edges of the ribbon
  geom_ribbon(colour=NA,alpha=0.1,
              aes(ymin=lower,ymax=upper))+ facet_wrap(vars(block_number))
  ## add rug plot based on original data
  geom_rug(data=ee$data,aes(y=NULL),sides="b")

#
