library('neatStats')
library('ggplot2')
library('mgcv')

setwd(path_neat())

fulldat = readRDS('increments_lgcit.rds')
fulldat = fulldat[fulldat$sect > 85,]
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
    geom_line(aes(color = "Probe")) +
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
            ymax = g_rtirrs_mean + g_rtirrs_cid,
            fill = "Control"
        ),
        alpha = 0.3
    ) +
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
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(r = 7, unit = "pt"))
    ) +
    ylab('Response time') + xlab('Trial number') + ggtitle('RTs per Item Type')

colrs_diffs = viridis::viridis(4, end = 0.75)
# barplot(rep(1, length(colrs_diffs)), col=colrs_diffs)

# MEAN AND SDs
p_diffs = ggplot(fulldat, aes(x = sect, y = g_rtdiffs_mean)) +
    # innocent SD
    geom_line(aes(y = i_rtdiffs_sd, color = "Innocent P-C predictor SD"), alpha = 0.8) +
    geom_ribbon(
        aes(
            y = i_rtdiffs_sd,
            ymin = i_rtdiffs_sd_lo,
            ymax = i_rtdiffs_sd_up,
            fill = "Innocent P-C predictor SD"
        ),
        alpha = 0.3
    ) +
    # guilty SD
    geom_line(aes(y = g_rtdiffs_sd, color = "Guilty P-C predictor SD"), alpha = 0.8) +
    geom_ribbon(
        aes(
            y = g_rtdiffs_sd,
            ymin = g_rtdiffs_sd_lo,
            ymax = g_rtdiffs_sd_up,
            fill = "Guilty P-C predictor SD"
        ),
        alpha = 0.3
    ) +
    # guilty mean
    geom_line(aes(color = "Guilty P-C predictor mean")) +
    geom_ribbon(
        aes(
            ymin = g_rtdiffs_mean - g_rtdiffs_cid,
            ymax = g_rtdiffs_mean + g_rtdiffs_cid,
            fill = "Guilty P-C predictor mean"
        ),
        alpha = 0.3
    ) +
    # etc
    scale_color_manual(
        "",
        breaks = c("Guilty P-C predictor mean", "Guilty P-C predictor SD", "Innocent P-C predictor SD"),
        values = c(
            "Guilty P-C predictor mean" = colrs_diffs[1],
            "Guilty P-C predictor SD" = colrs_diffs[3],
            "Innocent P-C predictor SD" = colrs_diffs[4]
        )
    ) +
    scale_fill_manual(
        "",
        breaks = c("Guilty P-C predictor mean", "Guilty P-C predictor SD", "Innocent P-C predictor SD"),
        values = c(
            "Guilty P-C predictor mean" = colrs_diffs[1],
            "Guilty P-C predictor SD" = colrs_diffs[3],
            "Innocent P-C predictor SD" = colrs_diffs[4]
        )
    ) +
    theme_bw() + theme(
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.text = element_text(face = 'plain', size = 12),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(r = 7, unit = "pt"))
    ) +
    ylab('Response time') + xlab('Trial number') + ggtitle('Means and SDs of Probe-Control RT Differences')

# AUCs
p_aucs = ggplot(fulldat, aes(x = sect, y = aucs)) +
    geom_line(color = '#0d0d26') +
    geom_ribbon(aes(ymin = aucs_lo,
                    ymax = aucs_up),
                alpha = 0.2,
                fill = '#0d0d26') +
    theme_bw() + theme(
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.text = element_text(face = 'plain', size = 12),
        legend.text = element_text(margin = margin(r = 7, unit = "pt"))
    ) +
    ylab('Area Under the ROC Curve') + xlab('Trial number') + ggtitle('Areas Under the ROC Curves')

p_items + ggtitle(NULL)
p_diffs + ggtitle(NULL)
p_aucs + ggtitle(NULL)

#### EXTRAPOLATE

subdat = fulldat
lm_g_mean = lm(g_rtdiffs_mean ~ sect, data = subdat)
lm_g_sd = lm(g_rtdiffs_sd ~ log(sect), data = subdat)
lm_i_sd = lm(i_rtdiffs_sd ~ log(sect), data = subdat)
lm_i_sd = gam(i_rtdiffs_sd ~ s(sect, k = 7), data = subdat) # default k = 5

# summary(lm_i_sd)
# report::report(lm_g_mean)
# report::report(lm_g_sd)
# report::report(lm_i_sd)

dat_extra <-
    data.frame(sect = seq(from = 10, to = 3000, by = 5),
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

fullpred = subdat
fullextra = dat_extra[dat_extra$sect < 2000,]

# predicted MEAN AND SDs
pdiff_extra = ggplot(fullpred, aes(x = sect, y = g_rtdiffs_mean)) +
    # predicted innocent SD
    geom_line(aes(y = pred_i_rtdiffs_sd),
              color = "darkgreen", linetype = "dotted",
              data = fullextra) +
    geom_ribbon(
        aes(y = pred_i_rtdiffs_sd,
            ymin = pred_i_rtdiffs_sd_lo,
            ymax = pred_i_rtdiffs_sd_up),
        data = fullextra,
        fill = "darkgreen",
        alpha = 0.3
    ) +
    # predicted guilty SD
    geom_line(aes(y = pred_g_rtdiffs_sd),
              color ='darkblue', linetype = "dotted",
              data = fullextra) +
    geom_ribbon(
        aes(y = pred_g_rtdiffs_sd,
            ymin = pred_g_rtdiffs_sd_lo,
            ymax = pred_g_rtdiffs_sd_up),
        data = fullextra,
        fill = 'darkblue',
        alpha = 0.3
    ) +
    # predicted mean
    geom_line(aes(y = pred_g_rtdiffs_mean),
              data = fullextra,
              color = 'darkred', linetype = "dotted",) +
    geom_ribbon(
        aes(y = pred_g_rtdiffs_mean,
            ymin = pred_g_rtdiffs_mean_lo,
            ymax = pred_g_rtdiffs_mean_up),
        data = fullextra,
        fill =  'darkred',
        alpha = 0.3
    ) +
    # innocent SD
    geom_line(aes(y = i_rtdiffs_sd, color = "Innocent P-C predictor SD")) +
    geom_ribbon(
        aes(
            y = i_rtdiffs_sd,
            ymin = i_rtdiffs_sd_lo,
            ymax = i_rtdiffs_sd_up,
            fill = "Innocent P-C predictor SD"
        ),
        alpha = 0.4
    ) +
    # guilty SD
    geom_line(aes(y = g_rtdiffs_sd, color = "Guilty P-C predictor SD")) +
    geom_ribbon(
        aes(
            y = g_rtdiffs_sd,
            ymin = g_rtdiffs_sd_lo,
            ymax = g_rtdiffs_sd_up,
            fill = "Guilty P-C predictor SD"
        ),
        alpha = 0.3
    ) +
    # guilty mean
    geom_line(aes(color = "Guilty P-C predictor mean")) +
    geom_ribbon(
        aes(
            ymin = g_rtdiffs_mean - g_rtdiffs_cid,
            ymax = g_rtdiffs_mean + g_rtdiffs_cid,
            fill = "Guilty P-C predictor mean"
        ),
        alpha = 0.3
    ) +
    # etc
    scale_color_manual(
        "",
        breaks = c("Guilty P-C predictor mean", "Guilty P-C predictor SD", "Innocent P-C predictor SD"),
        values = c(
            "Guilty P-C predictor mean" = colrs_diffs[1],
            "Guilty P-C predictor SD" = colrs_diffs[3],
            "Innocent P-C predictor SD" = colrs_diffs[4]
        )
    ) +
    scale_fill_manual(
        "",
        breaks = c("Guilty P-C predictor mean", "Guilty P-C predictor SD", "Innocent P-C predictor SD"),
        values = c(
            "Guilty P-C predictor mean" = colrs_diffs[1],
            "Guilty P-C predictor SD" = colrs_diffs[3],
            "Innocent P-C predictor SD" = colrs_diffs[4]
        )
    ) +
    theme_bw() + theme(
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.text = element_text(face = 'plain', size = 12),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(r = 7, unit = "pt"))
    ) +
    ylab('Response time') + xlab('Trial number') + ggtitle(NULL)


dcolors = "#440154FF" #viridis::viridis(11, end = 0.85)

### AUC sim
aucs_simmed = c()
for (section in dat_extra$sect) {
    g_m = dat_extra$pred_g_rtdiffs_mean[dat_extra$sect == section]
    g_sd = dat_extra$pred_g_rtdiffs_sd[dat_extra$sect == section]
    g_sd = max(c(g_sd, 1))
    i_sd = dat_extra$pred_i_rtdiffs_sd[dat_extra$sect == section]
    i_sd = max(c(i_sd, 1))
    aucs_simmed = c(
        aucs_simmed,
        neatStats::t_neat(
            bayestestR::distribution_normal(100,
                                            mean = g_m,
                                            sd = g_sd),
            bayestestR::distribution_normal(100,
                                            mean = 0,
                                            #sd = g_sd*0.5+7
                                            sd = i_sd
            ),
            hush = T,
            bf_added = F,
            auc_added = T
        )$stats['auc']
    )
}

dat_extra$pred_auc = aucs_simmed
fullextra = dat_extra[dat_extra$sect < 2000, ]
auc_maxes = fullextra[which.max(fullextra$pred_auc),]

paucs_extra = ggplot(fullpred, aes(x = sect,
                     y = aucs))  +  geom_segment(data = auc_maxes,
                                                 aes(
                                                     x = sect,
                                                     y = -Inf,
                                                     xend = sect,
                                                     yend = pred_auc
                                                 ), color = 'darkgrey',
                                                 size = 0.3) +
    geom_line(color = '#0d0d26') +
    geom_ribbon(aes(ymin = aucs_lo,
                    ymax = aucs_up),
                alpha = 0.2,
                fill = '#0d0d26') +
    geom_line(aes(y = pred_auc),
              data = fullextra,
              linetype = "dotted") +
    scale_color_manual(values = dcolors) +
    theme_bw() + theme(
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.text = element_text(face = 'plain', size = 12),
        legend.title = element_blank(),
        legend.text = element_text(margin = margin(r = 7, unit = "pt"))
    ) +  ylab('Area Under the Curve') +
    xlab('Trial number') + ggtitle(NULL) +
    geom_point(data = auc_maxes, aes(x = sect, y = pred_auc))


ggpubr::ggarrange(
    plotlist = list(
        pdiff_extra + ggtitle('Response time data extrapolation'),
        paucs_extra + ggtitle('Area Under the Curve extrapolation')
    ),
    heights = c(4, 3),
    labels = c('A', 'B'),
    label.y = 1.02,
    label.x = 0.02,
    ncol = 1#, legend = 'bottom'
)
