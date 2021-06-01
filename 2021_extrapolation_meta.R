library('neatStats')
library('ggplot2')
library('mgcv')

setwd(path_neat())

cit_trials = readRDS('meta_all_trials.rds')

fulldat = readRDS('increments_meta.rds')

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
    ) + facet_wrap(vars(dataset)) +
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

fullpredmerg = data.frame()
fullextramerg = data.frame()
for (dsetname in unique(fulldat$dataset[fulldat$dataset != 'dataset 9'])) {
    # dsetname = "dataset 1"
    subdat = fulldat[fulldat$dataset == dsetname, ]

    lm_g_mean = lm(g_rtdiffs_mean ~ sect, data = subdat)
    if (dsetname %in% c("dataset 5")) {
        lm_g_sd = gam(g_rtdiffs_sd ~ s(sect, k = 5), data = subdat)
    } else {
        lm_g_sd = lm(g_rtdiffs_sd ~ log(sect), data = subdat)
    }
    #lm_g_sd = gam(g_rtdiffs_sd ~ s(sect, k = 6), data = subdat)
    if (dsetname %in% c("dataset 1", "dataset 2")) {
        lm_i_sd = gam(i_rtdiffs_sd ~ s(sect, k = 7), data = subdat) # default k = 5
    } else if (dsetname %in% c("dataset 4", "dataset 10", "dataset 11")) {
        lm_i_sd = gam(i_rtdiffs_sd ~ s(sect, k = 12), data = subdat)
    } else if (dsetname %in% c("dataset 8")) {
        lm_i_sd = gam(i_rtdiffs_sd ~ s(sect, k = 22), data = subdat)
    } else {
        lm_i_sd = lm(i_rtdiffs_sd ~ log(sect), data = subdat)
    }
    dat_extra <-
        data.frame(sect = seq(from = 10, to = 13000, by = 10),
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

    aucs_simmed = c()
    for (section in dat_extra$sect) {
        g_m = dat_extra$pred_g_rtdiffs_mean[dat_extra$sect == section]
        g_sd = dat_extra$pred_g_rtdiffs_sd[dat_extra$sect == section]
        g_sd = max(c(g_sd, 1))
        i_sd = dat_extra$pred_i_rtdiffs_sd[dat_extra$sect == section]
        i_sd = max(c(i_sd, 1)) # g_sd * 0.5
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
    dat_extra$dataset = subdat$dataset[1]
    fullpredmerg = rbind(fullpredmerg, subdat)
    fullextramerg = rbind(fullextramerg, dat_extra)
}

fullpred = fullpredmerg
fullextra = fullextramerg[fullextramerg$sect < 600,]


# predicted MEAN AND SDs
ggplot(fullpred, aes(x = sect, y = g_rtdiffs_mean)) +
    # predicted innocent SD
    geom_line(aes(y = pred_i_rtdiffs_sd),
              color = "green",
              data = fullextra) +
    # geom_ribbon(
    #     aes(y = pred_i_rtdiffs_sd,
    #         ymin = pred_i_rtdiffs_sd_lo,
    #         ymax = pred_i_rtdiffs_sd_up),
    #     data = fullextra,
    #     fill = "green",
    #     alpha = 0.3
    # ) +
    # predicted guilty SD
    geom_line(aes(y = pred_g_rtdiffs_sd),
              color ='blue',
              data = fullextra) +
    geom_ribbon(
        aes(y = pred_g_rtdiffs_sd,
            ymin = pred_g_rtdiffs_sd_lo,
            ymax = pred_g_rtdiffs_sd_up),
        data = fullextra,
        fill = 'blue',
        alpha = 0.3
    ) +
    # predicted mean
    geom_line(aes(y = pred_g_rtdiffs_mean),
              data = fullextra,
              color = 'red') +
    geom_ribbon(
        aes(y = pred_g_rtdiffs_mean,
            ymin = pred_g_rtdiffs_mean_lo,
            ymax = pred_g_rtdiffs_mean_up),
        data = fullextra,
        fill =  'red',
        alpha = 0.3
    ) +
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
    ylab('Response time') + xlab('Trial number') + ggtitle('Means and SDs of Probe-Control RT Differences') + facet_wrap(vars(dataset))

# AUC extra


dcolors = viridis::viridis(11, end = 0.85)
fullextra = fullextramerg[fullextramerg$sect < 2000, ]

auc_maxes = do.call(rbind, lapply(split(
    fullextra, as.factor(fullextra$dataset)
), function(x) {
    return(x[which.max(x$pred_auc),])
}))

ggplot(fullpred, aes(
    x = sect,
    y = aucs,
    color = dataset,
    group = dataset
))  +  geom_segment(data = auc_maxes,
                    aes(
                        x = sect,
                        y = -Inf,
                        xend = sect,
                        yend = pred_auc
                    ),
                    size = 0.3) +
    geom_line() + ylim(0.5, NA) +
    geom_line(aes(y = pred_auc),
              data = fullextra,
              linetype = "dotted") +
    scale_color_manual(values = dcolors) +
    theme_bw() + theme(
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.text = element_text(face = 'plain', size = 12),
        legend.title = element_blank()
    ) +  ylab('Area Under the Curve') +
    xlab('Trial number') + ggtitle(NULL) +
    geom_point(data = auc_maxes, aes(x = sect, y = pred_auc))

