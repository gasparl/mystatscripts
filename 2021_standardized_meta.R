

library('metafor')
library('neatStats')
library("ggpubr")

setwd(path_neat())

metdat = readRDS("standard_aucs_meta.rds")
preds_metdat = readRDS("standard_predictors_meta.rds")

versionnames = c(
  'rt_mean_diff',
  'st_half_rt_diff',
  'st_half_rt_dcit',
  'st_half_rt_scaled',
  'st_blocked_rt_diff',
  'st_blocked_rt_dcit',
  'st_blocked_rt_scaled'
)
v_dict = c(
  'rt_mean_diff' = 'mean P-C difference',
  'st_half_rt_diff' = 'mean P-C difference (halves)',
  'st_half_rt_dcit' = 'standardized P-C difference (halves)',
  'st_half_rt_scaled' = 'standardized Probe RT (halves)',
  'st_blocked_rt_diff' = 'mean P-C difference (blocks)',
  'st_blocked_rt_dcit' = 'standardized P-C difference (blocks)',
  'st_blocked_rt_scaled' = 'standardized Probe RT (blocks)'
)
## -- Accuracies - cross-validated

accs_cv = NULL
for (pred_type in versionnames) {
    met_thres = metdat[metdat$version == pred_type, ]

    for (stud in unique(met_thres$dataset)) {
        thres_orig = met_thres$thresholds[met_thres$dataset == stud]
        thres_mean = mean(met_thres$thresholds[met_thres$dataset != stud])
        thres_median = median(met_thres$thresholds[met_thres$dataset != stud])
        preds_stud = preds_metdat[preds_metdat$dataset == stud, ]
        preds_guilty = preds_stud[[pred_type]][preds_stud$cond == 1]
        preds_innocent = preds_stud[[pred_type]][preds_stud$cond == 0]
        tpr = length(preds_guilty[preds_guilty > thres_orig]) / length(preds_guilty)
        tnr = length(preds_innocent[preds_innocent < thres_orig]) / length(preds_innocent)
        acc = (tpr + tnr) / 2

        tpr_mean = length(preds_guilty[preds_guilty > thres_mean]) / length(preds_guilty)
        tnr_mean = length(preds_innocent[preds_innocent < thres_mean]) / length(preds_innocent)
        acc_mean = (tpr_mean + tnr_mean) / 2

        tpr_med = length(preds_guilty[preds_guilty > thres_median]) / length(preds_guilty)
        tnr_med = length(preds_innocent[preds_innocent < thres_median]) / length(preds_innocent)
        acc_med = (tpr_med + tnr_med) / 2
        new_accs_cv = data.frame(
            dataset = stud,
            version = pred_type,
            acc_orig = acc,
            acc_cv_mean = acc_mean,
            acc_cv_med = acc_med,
            TPs_orig = tpr,
            TNs_orig = tnr,
            TPs_cv_mean = tpr_mean,
            TNs_cv_mean = tnr_mean,
            TPs_cv_med = tpr_med,
            TNs_cv_med = tnr_med
        )
        if (is.null(accs_cv)) {
            accs_cv = new_accs_cv
        } else {
            accs_cv = rbind(accs_cv, new_accs_cv)
        }
    }
}

aggr_neat(accs_cv, values = "acc_orig", group_by = c("version"), method = 'mean+sd', round_to = 3)
aggr_neat(accs_cv, values = "acc_cv_mean", group_by = c("version"), method = 'mean+sd', round_to = 3)
aggr_neat(accs_cv, values = "acc_cv_med", group_by = c("version"), method = 'mean+sd', round_to = 3)

aggr_neat(accs_cv, values = "TPs_orig", group_by = c("version"), method = 'mean+sd', round_to = 3)
aggr_neat(accs_cv, values = "TPs_cv_mean", group_by = c("version"), method = 'mean+sd', round_to = 3)
aggr_neat(accs_cv, values = "TPs_cv_med", group_by = c("version"), method = 'mean+sd', round_to = 3)

aggr_neat(accs_cv, values = "TNs_orig", group_by = c("version"), method = 'mean+sd', round_to = 3)
aggr_neat(accs_cv, values = "TNs_cv_mean", group_by = c("version"), method = 'mean+sd', round_to = 3)
aggr_neat(accs_cv, values = "TNs_cv_med", group_by = c("version"), method = 'mean+sd', round_to = 3)


# 'rt_mean_diff',
# 'st_half_rt_diff',
# 'st_half_rt_dcit',
# 'st_half_rt_scaled',
# 'st_blocked_rt_diff',
# 'st_blocked_rt_dcit',
# 'st_blocked_rt_scaled'

accs_cv_for_aov = accs_cv
accs_cv_for_aov$version = as.character(accs_cv_for_aov$version)
accs_cv_wide = reshape(
    as.data.frame(accs_cv_for_aov[accs_cv_for_aov$version %in% c("rt_mean_diff", "st_half_rt_dcit", "st_half_rt_scaled", "st_blocked_rt_dcit", "st_blocked_rt_scaled"), c(
        "dataset",
        'version',
        'acc_orig',
        'acc_cv_mean',
        'acc_cv_med'
    )]),
    idvar = "dataset",
    timevar = "version",
    direction = "wide"
)
# cat(names(accs_cv_wide), sep = "', '", fill = T)
anova_neat(
    accs_cv_wide,
    values = paste0('acc_orig.', c("rt_mean_diff", "st_half_rt_dcit", "st_half_rt_scaled", "st_blocked_rt_dcit", "st_blocked_rt_scaled")), bar_colors = c('#cc0000','#b3b3ff'),
    plot_means = TRUE,
    panel = 'acc_type',
    factor_names = c(orig_vs_cv = 'Cutoff', pred_type = ''),
    value_names = c(
        p_vs_i_basic = 'MPID',
        p_vs_i_scaled_items = 'SPRT',
        d_cit_pooled = 'SPID',
        '_orig' = 'Optimal',
        '_mean' = 'Inferred',
        TPs_ = 'True positive rate',
        TNs_ = 'True negative rate'
    )
) + theme(text = element_text(size = 21)) + scale_y_continuous(breaks = c(
    "0" = 0,
    ".25" = 0.25,
    ".50" = 0.5,
    ".75" = 0.75
))

## -- Meta-analysis

## sim Fig
# versionnames
vfacts = c(
  'rt_mean_diff',
  'st_half_rt_dcit',
  'st_half_rt_scaled',
  'st_blocked_rt_dcit',
  'st_blocked_rt_scaled'
)
fig_dat = metdat[metdat$version %in% vfacts, c("version", "dataset", "aucs", "auc_lower", "auc_upper")]
fig_dat$version = v_dict[fig_dat$version]
fig_dat$dataset = as.factor(fig_dat$dataset)
ggplot2::ggplot(data = fig_dat, aes(x = dataset,
                                    y = aucs,
                                    fill = version)) +
    geom_bar(stat = "identity", color = 'black',
             position = position_dodge(0.9)) +
    scale_fill_manual(values = viridis::viridis(length(vfacts), end = .9), name = NULL) +
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
    ylab("Area under the curve") + xlab("Dataset number") +
    theme(
        panel.grid.major.y = element_line(color = "#d5d5d5"),
        panel.grid.minor.y = element_line(color = "#d5d5d5"),
        legend.position = "bottom",
        text = element_text(family = "serif", size = 17)
    )


### META-ANALYSES

aggr_neat(metdat, cohens_d, method = "mean+sd", group_by = 'version')
aggr_neat(metdat, aucs, method = "mean+sd", group_by = 'version', round_to = 3)

# met_stat = metdat[metdat$version %in% c("p_vs_i", "d_cit", "p_vs_i_scaled_trials"),]
met_stat = metdat[metdat$version %in% vfacts,]

met_stat = met_stat[order(met_stat$dataset, met_stat$version),]

met_stat$multiple_single[met_stat$multiple_single == 'multiple'] = 'MP'
met_stat$multiple_single[met_stat$multiple_single == 'single'] = 'SP'
met_stat$multiple_single[met_stat$multiple_single == 'inducer'] = 'SPF'

met_stat$crowdsourced = "Yes"
met_stat$crowdsourced[grepl( "Noordraven & Verschuere", met_stat$study )] = "No"
met_stat$crowdsourced[grepl( "Verschuere & Kleinberg (2015)", met_stat$study, fixed = T )] = "No"
met_stat$crowdsourced[grepl( "Geven", met_stat$study, fixed = T )] = "No"

REML_multi <-
    rma(
        cohens_d,
        variance_d,
        data = met_stat,
        method = "REML",
        mods = ~ relevel(factor(version), ref = "rt_mean_diff") + relevel(factor(multiple_single), ref = "SP") + crowdsourced #, level = 0.9
    )
REML_multi

# here the tests for multi-level factors
anova(REML_multi, btt=2:4)

forest(
    REML_multi,
    psize = 1,
    xlab = NULL,
    xlim = c(-10, 5.9),
    slab = met_stat$study,
    ilab = cbind(
        met_stat$multiple_single,
        met_stat$crowdsourced,
        met_stat$version
    ),
    ilab.xpos = c(-4.0, -2.6, -1.3),
    cex = 0.75,
    fonts = 'serif'
)  # plot it


## ANOVA for AUC across different predictors

met_stat_wide = data.frame(met_stat)
met_stat_wide = reshape(
    as.data.frame(met_stat_wide[met_stat_wide$version %in% vfacts, c(
        "dataset",
        'version',
        'aucs',
        'auc_lower',
        'auc_upper'
    )]),
    idvar = "dataset",
    timevar = "version",
    direction = "wide"
)

aggr_neat(met_stat, aucs, method = "mean+sd", group_by = 'version', round_to = 3)
anova_neat(
    met_stat_wide,
    values = paste0('aucs.', c("rt_mean_diff", "st_half_rt_dcit", "st_half_rt_scaled", "st_blocked_rt_dcit", "st_blocked_rt_scaled")),
    within_ids = 'predictor_version', plot_means = TRUE
)

