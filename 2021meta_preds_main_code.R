

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
source("2021meta_preds_functions.R")


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
  dat_i_prep$rt_mean_h1_diff = dat_i_prep$rt_mean_h1_probe - dat_i_prep$rt_mean_h1_irrelevant
  dat_i_prep$rt_mean_h2_diff = dat_i_prep$rt_mean_h2_probe - dat_i_prep$rt_mean_h2_irrelevant

  # Get the cohens d and stuff
  eff_data <-
      effectsize_data(
          dat_i_prep$id,
          dat_i_prep$rt_mean_diff,
          dat_i_prep$rt_mean_h1_diff,
          dat_i_prep$rt_mean_h2_diff,
          dat_i_prep$cond,
          dat_i_prep$multiple_single,
          dat_i_prep$study,
          sd_i
      )

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

# saveRDS(all_predicts, "half_predictors_meta.rds")
# saveRDS(metdat, "half_aucs_meta.rds")



## -- Accuracies - cross-validated

aggr_neat(metdat, values = "accuracies", group_by = c("version"), method = 'mean+sd')
aggr_neat(metdat, values = "aucs", group_by = c("version"), method = 'mean+sd')

plot_neat(
  metdat,
    values = c(
        'aucs'
    ),
    between_vars = 'version'
)

plot_neat(
  all_predicts,
  values = c(
    'rt_mean_h1_diff', 'rt_mean_h2_diff'
  ),  between_vars = 'dataset', type = 'bar'
)

# anova_neat(
#   all_predicts,
#   values = c(
#     'rt_mean_h1_diff', 'rt_mean_h2_diff'
#   ),  between_vars = 'study'
# )

## -- Meta-analysis

## sim Fig
# p_vs_i
fig_dat = metdat[metdat$version %in% c("pred_1", "pred_2"), c("version", "dataset", "aucs", "auc_lower", "auc_upper")]
fig_dat$dataset = as.factor(fig_dat$dataset)
ggplot2::ggplot(data = fig_dat, aes(x = dataset,
                                    y = aucs,
                                    fill = version)) +
    geom_bar(stat = "identity",
             position = position_dodge(0.9)) +
    scale_fill_manual(values = c('#AAAAAA', '#333333'), name = NULL) +
    geom_errorbar(aes(
        ymin = fig_dat$auc_lower,
        ymax = fig_dat$auc_upper,
        width = 0.2
    ),
    position = position_dodge(0.9)) + theme_bw() +
    theme(panel.grid.major.x = element_blank())  +
    scale_y_continuous(breaks = c(
        "0" = 0,
        ".25" = 0.25,
        ".50" = 0.5,
        ".75" = 0.75
    )) +
    ylab("Area under the curve") + xlab("Dataset (individual experimental design)") +
    theme(
        panel.grid.major.y = element_line(color = "#d5d5d5"),
        panel.grid.minor.y = element_line(color = "#d5d5d5"),
        legend.position = "bottom",
        text = element_text(family = "serif", size = 17)
    )


### META-ANALYSES

aggr_neat(metdat, cohens_d, method = "mean+sd", group_by = 'version')
aggr_neat(metdat, aucs, method = "mean+sd", group_by = 'version')

met_stat = metdat[metdat$version %in% c("p_vs_i","p_vs_i_per_tvsi"),]

met_stat = met_stat[order(met_stat$dataset, met_stat$version),]

met_stat$multiple_single[met_stat$multiple_single == 'multiple'] = 'MP'
met_stat$multiple_single[met_stat$multiple_single == 'single'] = 'SP'
met_stat$multiple_single[met_stat$multiple_single == 'inducer'] = 'SPF'

met_stat$crowdsourced = "Yes"
met_stat$crowdsourced[grepl( "Noordraven & Verschuere", met_stat$study )] = "No"
met_stat$crowdsourced[grepl( "Verschuere & Kleinberg (2015)", met_stat$study, fixed = T )] = "No"

# reshape(
#     as.data.frame(met_stat[, c("study", 'version', 'aucs')]),
#     idvar = "study",
#     timevar = "version",
#     direction = "wide"
# )
# thresholds = reshape(
#     as.data.frame(met_stat[, c("study", 'version', 'thresholds')]),
#     idvar = "study",
#     timevar = "version",
#     direction = "wide"
# )

# met_stat$multiple_single[met_stat$multiple_single == "inducer"] = "multiple"

REML_multi <-
    rma(
        cohens_d,
        variance_d,
        data = met_stat,
        method = "REML",
        mods = ~ relevel(factor(version), ref = "p_vs_i") + relevel(factor(multiple_single), ref = "SP") + crowdsourced #, level = 0.9
    )
REML_multi
## this is to compare pairwise the third pair
REML_multi <-
    rma(
        cohens_d,
        variance_d,
        data = met_stat,
        method = "REML",
        mods = ~ relevel(factor(version), ref = "d_cit_pooled") + relevel(factor(multiple_single), ref = "MP") + crowdsourced #, level = 0.9
    )
REML_multi
## this is to compare pairwise the third pair
REML_multi <-
    rma(
        cohens_d,
        variance_d,
        data = met_stat,
        method = "REML",
        mods = ~ relevel(factor(version), ref = "p_vs_i") + relevel(factor(multiple_single), ref = "MP") + crowdsourced #, level = 0.9
    )
REML_multi
# here the tests for multi-level factors
anova(REML_multi, btt=2:3)
anova(REML_multi, btt=4:5)
anova(REML_multi, btt=3:4)

forest(
    REML_multi,
    psize = 1,
    xlab = NULL,
    xlim = c(-10, 5.9),
    slab = met_stat$study,
    ilab = cbind(
        met_stat$multiple_single,
        met_stat$crowdsourced,
        met_stat$simulated
    ),
    ilab.xpos = c(-4.0, -2.6, -1.3),
    cex = 0.75,
    fonts = 'serif'
)  # plot it

text(
    x = c(-9.1, -4.1, -2.7, -1.2, 3.0),
    y = 24,
    labels = c(
        "Dataset title",
        "Protocol",
        "Crowdsourced",
        "Simulated",
        "Effect Size (Cohen's d) and 95% CI"
    ),
    font = 2,
    family = 'serif',
    cex = .9
)


## t-test for AUC

t_neat(metdat$aucs[metdat$version == 'simulated'],
       metdat$aucs[metdat$version == 'p_vs_i'],
       pair = T,
       round_descr = 3)

corr_neat(metdat$aucs[metdat$version == "p_vs_i"], metdat$aucs[metdat$version == "simulated"])

weights::wtd.cor(metdat$aucs[metdat$version == "p_vs_i"], metdat$aucs[metdat$version == "simulated"], weight = (sd_metdat$n_g[sd_metdat$version == "p_vs_i"]+sd_metdat$n_i[sd_metdat$version == "p_vs_i"]))


### JOINT TABLES

unique(out_aucs$version)
unique(out_descriptives$version)
unique(out_main$version)
names(out_aucs)

out_main = metdat
out_descriptives = sd_metdat
out_aucs = accs_cv
out_main = out_main[out_main$version %in% c("d_cit_pooled", "p_vs_i_per_tvsi", "p_vs_i", "simulated"), !(names(out_main) %in% c("eff_p_i_control"))]
out_aucs = out_aucs[out_aucs$version %in% c("d_cit_pooled", "p_vs_i_per_tvsi", "p_vs_i", "simulated"),  !(names(out_aucs) %in% c("eff_p_i_control"))]

out_main$crowdsourced = "Yes"
out_main$crowdsourced[grepl( "Noordraven & Verschuere", out_main$study )] = "No"
out_main$crowdsourced[grepl( "Verschuere & Kleinberg (2015)", out_main$study, fixed = T )] = "No"

out_full = merge(out_main, out_descriptives, all = TRUE)
out_full = merge(out_full, out_aucs, all = TRUE)

# save(out_full, file="cit_meta_data_aggregated_EQUAL_SIM_SAMPLE.Rda")

out_full_10000 = out_full

# save(out_full_10000, file="cit_meta_data_aggregated_10000_SIM_SAMPLE.Rda")
