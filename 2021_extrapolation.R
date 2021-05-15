library('neatStats')
library('ggplot2')
library('mgcv')

setwd(path_neat())
sd_c_l = function(n) {
    sqrt((n - 1) / qchisq(0.05 / 2, n - 1, lower.tail = FALSE))
}
sd_c_u  = function(n) {
    sqrt((n - 1) / qchisq(0.05 / 2, n - 1, lower.tail = TRUE))
}

# cit_trials = readRDS('lgcit_trials_both.rds')
cit_trials = readRDS('meta_all_trials.rds')
cit_trials = cit_trials[cit_trials$dataset == 'dataset 11',] # 11

cit_trials = cit_trials[cit_trials$valid_trial == 1,]
#cit_trials = cit_trials[cit_trials$study == 'study1',]

sections = seq(from = 100, to = max(cit_trials$trial_number), by = 15) # 30

dat_sects = data.frame(
    sect = numeric(),
    g_rtprobs_mean = numeric(),
    g_rtprobs_cid = numeric(),
    g_rtirrs_mean = numeric(),
    g_rtirrs_cid = numeric(),
    g_rttargs_mean = numeric(),
    g_rttargs_cid = numeric(),

    g_rtdiffs_mean = numeric(),
    g_rtdiffs_cid = numeric(),
    g_rtdiffs_sd = numeric(),
    g_rtdiffs_sd_up = numeric(),
    g_rtdiffs_sd_lo = numeric(),

    i_rtprobs_mean = numeric(),
    i_rtprobs_cid = numeric(),
    i_rtirrs_mean = numeric(),
    i_rtirrs_cid = numeric(),
    i_rttargs_mean = numeric(),
    i_rttargs_cid = numeric(),

    i_rtdiffs_mean = numeric(),
    i_rtdiffs_cid = numeric(),
    i_rtdiffs_sd = numeric(),
    i_rtdiffs_sd_up = numeric(),
    i_rtdiffs_sd_lo = numeric(),

    aucs = numeric(),
    aucs_lo = numeric(),
    aucs_up = numeric(),
    stringsAsFactors = FALSE
)

for (cutoff in sections) {
    # cutoff = 30
    subdat = cit_trials[cit_trials$trial_number < cutoff, ]
    g_subdat = subdat[subdat$guilt == 'guilty',]
    g_rt_probs = c()
    g_rt_irrs = c()
    g_rt_targs = c()
    g_rt_diffs = c()
    for (subjectid in unique(g_subdat$subject_id)) {
        # subjectid =  "id999"
        gdat = g_subdat[g_subdat$subject_id == subjectid,]
        m_prob = mean(gdat$rt_start[gdat$stim_type == 'probe'])
        m_irr = mean(gdat$rt_start[gdat$stim_type == 'control'])
        g_rt_targs = c(g_rt_targs, mean(gdat$rt_start[gdat$stim_type == 'target']))
        g_rt_probs = c(g_rt_probs, m_prob)
        g_rt_irrs = c(g_rt_irrs, m_irr)
        g_rt_diffs = c(g_rt_diffs, m_prob - m_irr)
    }
    i_subdat = subdat[subdat$guilt == 'innocent',]
    i_rt_probs = c()
    i_rt_irrs = c()
    i_rt_targs = c()
    i_rt_diffs = c()
    for (subjectid in unique(i_subdat$subject_id)) {
        # subjectid = c(0, "CAN_20200620100940_hu")
        idat = i_subdat[i_subdat$subject_id == subjectid, ]
        m_prob = mean(idat$rt_start[idat$stim_type == 'probe'])
        m_irr = mean(idat$rt_start[idat$stim_type == 'control'])
        i_rt_targs = c(i_rt_targs, mean(idat$rt_start[idat$stim_type == 'target']))
        i_rt_probs = c(i_rt_probs, m_prob)
        i_rt_irrs = c(i_rt_irrs, m_irr)
        i_rt_diffs = c(i_rt_diffs, m_prob - m_irr)
    }
    auc_stat = as.numeric(pROC::ci.auc(
        t_neat(g_rt_diffs, i_rt_diffs, auc_added = TRUE, hush = TRUE)$roc_obj,
        conf.level = 0.95
    ))


    dat_sects[nrow(dat_sects) + 1, ] = c(
        sect = cutoff,
        g_rtprobs_mean = mean(g_rt_probs),
        g_rtprobs_cid = mean_ci(g_rt_probs),
        g_rtirrs_mean = mean(g_rt_irrs),
        g_rtirrs_cid = mean_ci(g_rt_irrs),
        g_rttargs_mean = mean(g_rt_targs),
        g_rttargs_cid = mean_ci(g_rt_targs),

        g_rtdiffs_mean = mean(g_rt_diffs),
        g_rtdiffs_cid = mean_ci(g_rt_diffs),
        g_rtdiffs_sd = sd(g_rt_diffs),
        g_rtdiffs_sd_up = sd(g_rt_diffs) * sd_c_l(length(g_rt_diffs)),
        g_rtdiffs_sd_lo = sd(g_rt_diffs) * sd_c_u(length(g_rt_diffs)),

        i_rtprobs_mean = mean(i_rt_probs),
        i_rtprobs_cid = mean_ci(i_rt_probs),
        i_rtirrs_mean = mean(i_rt_irrs),
        i_rtirrs_cid = mean_ci(i_rt_irrs),
        i_rttargs_mean = mean(i_rt_targs),
        i_rttargs_cid = mean_ci(i_rt_targs),

        i_rtdiffs_mean = mean(i_rt_diffs),
        i_rtdiffs_cid = mean_ci(i_rt_diffs),
        i_rtdiffs_sd = sd(i_rt_diffs),
        i_rtdiffs_sd_up = sd(i_rt_diffs) * sd_c_l(length(i_rt_diffs)),
        i_rtdiffs_sd_lo = sd(i_rt_diffs) * sd_c_u(length(i_rt_diffs)),

        aucs = auc_stat[2],
        aucs_lo = auc_stat[1],
        aucs_up = auc_stat[3]
    )
}

fulldat = dat_sects # #[dat_sects$sect > 200,]
## look at the data

colrs_items = viridis::plasma(3, end = 0.9)

ggplot(fulldat, aes(x = sect, y = g_rtprobs_mean)) +
    # targets
    geom_line(aes(y = g_rttargs_mean)) +
    geom_line(aes(y = g_rttargs_mean), color = colrs_items[3]) +
    geom_ribbon(
        aes(y = g_rttargs_mean,
            ymin = g_rttargs_mean -g_rttargs_cid,
            ymax = g_rttargs_mean+g_rttargs_cid
        ),
        fill = colrs_items[3], alpha = 0.3
    )+
    # probe
    geom_line(color = colrs_items[2]) +
    geom_ribbon(
        aes(
            ymin = g_rtprobs_mean - g_rtprobs_cid,
            ymax = g_rtprobs_mean + g_rtprobs_cid
        ), fill = colrs_items[2],
        alpha = 0.3
    )  +
    # innocent SD
    geom_line(aes(y = g_rtirrs_mean)) +
    geom_line(aes(y = g_rtirrs_mean), color = colrs_items[1]) +
    geom_ribbon(
        aes(y = g_rtirrs_mean,
            ymin = g_rtirrs_mean - g_rtirrs_cid,
            ymax = g_rtirrs_mean + g_rtirrs_cid),
        fill = colrs_items[1],
        alpha = 0.3
    ) + # facet_wrap(vars(dataset)) +
    theme_bw() + theme(strip.background = element_blank(), plot.title = element_text(hjust = 0.5),
                       strip.text = element_text(face = 'plain', size = 12), legend.position = "bottom",
                       legend.title = element_blank()) +
    ylab('Response time') + xlab('Trial number') + ggtitle('RTs per Item Type')

colrs_diffs = viridis::viridis(3, end = 0.85)

ggplot(fulldat, aes(x = sect, y = g_rtdiffs_mean)) +
    geom_line(color = colrs_diffs[3]) +
    geom_ribbon(
        aes(
            ymin = g_rtdiffs_mean - g_rtdiffs_cid,
            ymax = g_rtdiffs_mean + g_rtdiffs_cid
        ),
        alpha = 0.3, fill = colrs_diffs[3]
    ) +
    # guilty SD
    geom_line(aes(y = g_rtdiffs_sd), color = colrs_diffs[2]) +
    geom_ribbon(
        aes(y = g_rtdiffs_sd,
            ymin = g_rtdiffs_sd_lo,
            ymax = g_rtdiffs_sd_up
        ),
        alpha = 0.3, fill = colrs_diffs[2]
    ) +
    # innocent SD
    geom_line(aes(y = i_rtdiffs_sd), color = colrs_diffs[1]) +
    geom_ribbon(
        aes(y = i_rtdiffs_sd,
            ymin = i_rtdiffs_sd_lo,
            ymax = i_rtdiffs_sd_up
        ),
        alpha = 0.3, fill = colrs_diffs[1]
    ) +
    theme_bw() + theme(
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.text = element_text(face = 'plain', size = 12)
    ) +
    ylab(NULL) + xlab('Trial number') + ggtitle('Means and SDs of Probe-Control RT Differences')




# EXTRAPOLATE
lm_g_mean = lm(g_rtdiffs_mean ~ sect, data = fulldat)
# summary(lm_g_mean)

lm_g_sd = lm(g_rtdiffs_sd ~ log(sect), data = fulldat)
#lm_g_sd = gam(g_rtdiffs_sd ~ s(sect, k = 6), data = fulldat)

lm_i_sd = lm(i_rtdiffs_sd ~ log(sect), data = fulldat)
#lm_i_sd = gam(i_rtdiffs_sd ~ s(sect, k = 7), data = fulldat) # default k = 5

# summary(lm_g_mean)
# summary(lm_g_sd)
# summary(lm_i_sd)

## extrapolate based on model
dat_extra <- data.frame(sect = seq(from = 10, to = 3000, by = 1), pred_i_rtdiffs_mean = 0)
pre_g_rtdiffs_m = predict(lm_g_mean, newdata = dat_extra, se.fit = TRUE)
dat_extra$pred_g_rtdiffs_mean <- pre_g_rtdiffs_m$fit
dat_extra$pred_g_rtdiffs_mean_up <- pre_g_rtdiffs_m$fit + pre_g_rtdiffs_m$se.fit*1.96
dat_extra$pred_g_rtdiffs_mean_lo <- pre_g_rtdiffs_m$fit - pre_g_rtdiffs_m$se.fit*1.96

pre_g_rtdiffs_sd = predict(lm_g_sd, newdata = dat_extra, se.fit = TRUE)
dat_extra$pred_g_rtdiffs_sd <- pre_g_rtdiffs_sd$fit
dat_extra$pred_g_rtdiffs_sd_up <- pre_g_rtdiffs_sd$fit + pre_g_rtdiffs_sd$se.fit*1.96
dat_extra$pred_g_rtdiffs_sd_lo <- pre_g_rtdiffs_sd$fit - pre_g_rtdiffs_sd$se.fit*1.96

pre_i_rtdiffs_sd = predict(lm_i_sd, newdata = dat_extra, se.fit = TRUE)
dat_extra$pred_i_rtdiffs_sd <- pre_i_rtdiffs_sd$fit
dat_extra$pred_i_rtdiffs_sd_up <- pre_i_rtdiffs_sd$fit + pre_i_rtdiffs_sd$se.fit*1.96
dat_extra$pred_i_rtdiffs_sd_lo <- pre_i_rtdiffs_sd$fit - pre_i_rtdiffs_sd$se.fit*1.96



### extrapolation


ggplot(fulldat, aes(x = sect, y = g_rtdiffs_mean)) +
    geom_line(color = colrs_diffs[3]) +
    geom_ribbon(
        aes(
            ymin = g_rtdiffs_mean - g_rtdiffs_cid,
            ymax = g_rtdiffs_mean + g_rtdiffs_cid
        ),
        alpha = 0.3, fill = colrs_diffs[3]
    ) +
    geom_line(aes(y = pred_g_rtdiffs_mean),
              data = dat_extra,
              color = colrs_diffs[2]) +
    geom_ribbon(
        aes(y = pred_g_rtdiffs_mean,
            ymin = pred_g_rtdiffs_mean_lo,
            ymax = pred_g_rtdiffs_mean_up
        ), data = dat_extra,
        fill = colrs_diffs[2], alpha = 0.3
    ) +
    # guilty SD
    geom_line(aes(y = g_rtdiffs_sd)) +
    geom_point(aes(y = g_rtdiffs_sd)) +
    geom_line(aes(y = pred_g_rtdiffs_sd), color = colrs_diffs[1], data = dat_extra) +
    geom_ribbon(
        aes(y = pred_g_rtdiffs_sd,
            ymin = pred_g_rtdiffs_sd_lo,
            ymax = pred_g_rtdiffs_sd_up
        ), data = dat_extra,
        fill = colrs_diffs[1], alpha = 0.3
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
    i_sd = max(c(i_sd, g_sd*0.5))
    aucs_simmed = c(aucs_simmed,
                    neatStats::t_neat(
        bayestestR::distribution_normal(
            100,
            mean = g_m,
            sd = g_sd
        ),
        bayestestR::distribution_normal(
            100,
            mean = 0,
            #sd = g_sd*0.5+7
            sd = i_sd
        ), hush = T,
        bf_added = F,
        auc_added = T
    )$stats['auc'])
}
dat_extra$pred_auc = aucs_simmed
max(dat_extra$pred_auc)
ggplot(fulldat, aes(x = sect, y = aucs)) +
    geom_line() +
    geom_point() +
    geom_hline(aes(yintercept = 0)) +
    geom_line(aes(y = pred_auc), color = "red", data = dat_extra)

