library('neatStats')
library('ggplot2')

setwd(path_neat())

cit_trials = readRDS('meta_all_trials.rds')

fulldat = readRDS('increments_meta.rds')
lmodels = readRDS('increments_lm_meta.rds')

# Figures

colrs_items = viridis::plasma(3, end = 0.9)

p_items = ggplot(fulldat, aes(x = sect, y = g_rtprobs_mean)) +
    # targets
    geom_line(aes(y = g_rttargs_mean)) +
    geom_line(aes(y = g_rttargs_mean, color = "Target")) +
    geom_ribbon(
        aes(
            y = g_rttargs_mean,
            ymin = g_rttargs_mean - g_rttargs_cid,
            ymax = g_rttargs_mean + g_rttargs_cid,
            fill = "Target"
        ),
        alpha = 0.3
    ) +
    # probe
    geom_line(aes(color ="Probe")) +
    geom_ribbon(
        aes(
            ymin = g_rtprobs_mean - g_rtprobs_cid,
            ymax = g_rtprobs_mean + g_rtprobs_cid,
            fill = "Probe"
        ),
        alpha = 0.3
    )  +
    # control
    geom_line(aes(y = g_rtirrs_mean, color = "Control")) +
    geom_ribbon(
        aes(
            y = g_rtirrs_mean,
            ymin = g_rtirrs_mean - g_rtirrs_cid,
            ymax = g_rtirrs_mean + g_rtirrs_cid, fill = "Control"
        ),
        alpha = 0.3
    ) + facet_wrap(vars(dataset)) +
    # etc
    scale_color_manual(
        "",
        breaks = c("Probe", "Control", "Target"),
        values = c(
            "Probe" = colrs_items[2],
            "Control" = colrs_items[1],
            "Target" = colrs_items[3]
        )
    ) +
    scale_fill_manual(
        "",
        breaks = c("Probe", "Control", "Target"),
        values = c(
            "Probe" = colrs_items[2],
            "Control" = colrs_items[1],
            "Target" = colrs_items[3]
        )
    ) +
    theme_bw() + theme(
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.text = element_text(face = 'plain', size = 12),
        legend.position = "top",
        legend.title = element_blank()
    ) +
    ylab('Response time') + xlab('Trial number') + ggtitle('RTs per Item Type')

colrs_diffs = viridis::viridis(4, end = 0.8)
# barplot(rep(1, length(colrs_diffs)), col=colrs_diffs)

# MEAN AND SDs
p_diffs = ggplot(fulldat, aes(x = sect, y = g_rtdiffs_mean)) +
    # innocent SD
    geom_line(aes(y = i_rtdiffs_sd, color = "Innocent P-C SD"), alpha = 0.8) +
    geom_ribbon(
        aes(y = i_rtdiffs_sd,
            ymin = i_rtdiffs_sd_lo,
            ymax = i_rtdiffs_sd_up,
            fill = "Innocent P-C SD"),
        alpha = 0.3
    ) + facet_wrap(vars(dataset)) +
    # guilty SD
    geom_line(aes(y = g_rtdiffs_sd, color = "Guilty P-C SD"), alpha = 0.8) +
    geom_ribbon(
        aes(y = g_rtdiffs_sd,
            ymin = g_rtdiffs_sd_lo,
            ymax = g_rtdiffs_sd_up,
            fill = "Guilty P-C SD"),
        alpha = 0.3
    ) +
    # guilty mean
    geom_line(aes(color ="Guilty P-C mean")) +
    geom_ribbon(
        aes(
            ymin = g_rtdiffs_mean - g_rtdiffs_cid,
            ymax = g_rtdiffs_mean + g_rtdiffs_cid,
            fill = "Guilty P-C mean"
        ),
        alpha = 0.3
    ) +
    # etc
    scale_color_manual(
        "",
        breaks = c("Guilty P-C mean", "Guilty P-C SD", "Innocent P-C SD"),
        values = c(
            "Guilty P-C mean" = colrs_diffs[1],
            "Guilty P-C SD" = colrs_diffs[3],
            "Innocent P-C SD" = colrs_diffs[2]
        )
    ) +
    scale_fill_manual(
        "",
        breaks = c("Guilty P-C mean", "Guilty P-C SD", "Innocent P-C SD"),
        values = c(
            "Guilty P-C mean" = colrs_diffs[1],
            "Guilty P-C SD" = colrs_diffs[3],
            "Innocent P-C SD" = colrs_diffs[2]
        )
    ) +
    theme_bw() + theme(
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.text = element_text(face = 'plain', size = 12),
        legend.position = "top",
        legend.title = element_blank()
    ) +
    ylab('Response time') + xlab('Trial number') + ggtitle('Means and SDs of Probe-Control RT Differences')

# AUCs
p_aucs = ggplot(fulldat, aes(x = sect, y = aucs)) +
    geom_line(color = '#0d0d26') +
    geom_ribbon(aes(ymin = aucs_lo,
                    ymax = aucs_up),
                alpha = 0.2,
                fill = '#0d0d26') + facet_wrap(vars(dataset)) +
    theme_bw() + theme(
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.text = element_text(face = 'plain', size = 12)
    ) +
    ylab('Area Under the ROC Curve') + xlab('Trial number') + ggtitle('Areas Under the ROC Curves')

p_items + ggtitle(NULL)
p_diffs + ggtitle(NULL)
p_aucs + ggtitle(NULL)

# FIG together
# ggpubr::ggarrange(
#     plotlist = list(p_items, p_diffs, p_aucs),
#     ncol = 1#, legend = 'bottom'
# ) # ratio: 700 x 2200


# EXTRAPOLATE

# summary(lm_g_mean)
# summary(lm_g_sd)
# summary(lm_i_sd)

## extrapolate based on model
dat_extra <-
    data.frame(sect = seq(from = 10, to = 3000, by = 1),
               pred_i_rtdiffs_mean = 0)
pre_g_rtdiffs_m = predict(lm_g_mean, newdata = dat_extra, se.fit = TRUE)
dat_extra$pred_g_rtdiffs_mean <- pre_g_rtdiffs_m$fit
dat_extra$pred_g_rtdiffs_mean_up <-
    pre_g_rtdiffs_m$fit + pre_g_rtdiffs_m$se.fit * 1.96
dat_extra$pred_g_rtdiffs_mean_lo <-
    pre_g_rtdiffs_m$fit - pre_g_rtdiffs_m$se.fit * 1.96

pre_g_rtdiffs_sd = predict(lm_g_sd, newdata = dat_extra, se.fit = TRUE)
dat_extra$pred_g_rtdiffs_sd <- pre_g_rtdiffs_sd$fit
dat_extra$pred_g_rtdiffs_sd_up <-
    pre_g_rtdiffs_sd$fit + pre_g_rtdiffs_sd$se.fit * 1.96
dat_extra$pred_g_rtdiffs_sd_lo <-
    pre_g_rtdiffs_sd$fit - pre_g_rtdiffs_sd$se.fit * 1.96

pre_i_rtdiffs_sd = predict(lm_i_sd, newdata = dat_extra, se.fit = TRUE)
dat_extra$pred_i_rtdiffs_sd <- pre_i_rtdiffs_sd$fit
dat_extra$pred_i_rtdiffs_sd_up <-
    pre_i_rtdiffs_sd$fit + pre_i_rtdiffs_sd$se.fit * 1.96
dat_extra$pred_i_rtdiffs_sd_lo <-
    pre_i_rtdiffs_sd$fit - pre_i_rtdiffs_sd$se.fit * 1.96

### extrapolation

ggplot(fulldat, aes(x = sect, y = g_rtdiffs_mean)) +
    geom_line(color = colrs_diffs[3]) +
    geom_ribbon(
        aes(
            ymin = g_rtdiffs_mean - g_rtdiffs_cid,
            ymax = g_rtdiffs_mean + g_rtdiffs_cid
        ),
        alpha = 0.3,
        fill = colrs_diffs[3]
    ) +
    geom_line(aes(y = pred_g_rtdiffs_mean),
              data = dat_extra,
              color = colrs_diffs[2]) +
    geom_ribbon(
        aes(y = pred_g_rtdiffs_mean,
            ymin = pred_g_rtdiffs_mean_lo,
            ymax = pred_g_rtdiffs_mean_up),
        data = dat_extra,
        fill = colrs_diffs[2],
        alpha = 0.3
    ) +
    # guilty SD
    geom_line(aes(y = g_rtdiffs_sd)) +
    geom_point(aes(y = g_rtdiffs_sd)) +
    geom_line(aes(y = pred_g_rtdiffs_sd), color = colrs_diffs[1], data = dat_extra) +
    geom_ribbon(
        aes(y = pred_g_rtdiffs_sd,
            ymin = pred_g_rtdiffs_sd_lo,
            ymax = pred_g_rtdiffs_sd_up),
        data = dat_extra,
        fill = colrs_diffs[1],
        alpha = 0.3
    ) +
    # innocent SD
    geom_line(aes(y = i_rtdiffs_sd)) +
    geom_point(aes(y = i_rtdiffs_sd)) +
    geom_line(aes(y = pred_i_rtdiffs_sd), color = "lightblue", data = dat_extra) +
    geom_ribbon(
        aes(y = pred_i_rtdiffs_sd,
            ymin = pred_i_rtdiffs_sd_lo,
            ymax = pred_i_rtdiffs_sd_up),
        data = dat_extra,
        fill = "lightblue",
        alpha = 0.3
    )



aucs_simmed = c()
for (section in dat_extra$sect) {
    g_m = dat_extra$pred_g_rtdiffs_mean[dat_extra$sect == section]
    g_sd = dat_extra$pred_g_rtdiffs_sd[dat_extra$sect == section]
    g_sd = max(c(g_sd, 1))
    i_sd = dat_extra$pred_i_rtdiffs_sd[dat_extra$sect == section]
    i_sd = max(c(i_sd, g_sd * 0.5))
    aucs_simmed = c(
        aucs_simmed,
        neatStats::t_neat(
            bayestestR::distribution_normal(100,
                                            mean = g_m,
                                            sd = g_sd),
            bayestestR::distribution_normal(100,
                                            mean = 0,
                                            #sd = g_sd*0.5+7
                                            sd = i_sd),
            hush = T,
            bf_added = F,
            auc_added = T
        )$stats['auc']
    )
}
dat_extra$pred_auc = aucs_simmed
max(dat_extra$pred_auc)
ggplot(fulldat, aes(x = sect, y = aucs)) +
    geom_line() +
    geom_point() +
    geom_hline(aes(yintercept = 0)) +
    geom_line(aes(y = pred_auc), color = "red", data = dat_extra)
