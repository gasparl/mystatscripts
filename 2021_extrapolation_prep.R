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
# cit_trials$dataset = 'Studies 2a and 2b'

cit_trials = readRDS('meta_all_trials.rds')
# cit_trials = cit_trials[cit_trials$dataset == 'dataset 11', ] # 11

cit_trials = cit_trials[cit_trials$valid_trial == 1, ]
#cit_trials = cit_trials[cit_trials$study == 'study1',]

dats_merged = data.frame()
for (dsetname in sort(unique(cit_trials$dataset))) {
    dset_trials = cit_trials[cit_trials$dataset == dsetname, ]
    sections = seq(
        from = 50,
        to = max(dset_trials$trial_number),
        by = 5
    ) # 30

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
        subdat = dset_trials[dset_trials$trial_number < cutoff, ]
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
            t_neat(
                g_rt_diffs,
                i_rt_diffs,
                auc_added = TRUE,
                hush = TRUE
            )$roc_obj,
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
    dat_sects$dataset = dsetname
    dats_merged = rbind(dats_merged, dat_sects)
}

fulldat = dats_merged # #[dat_sects$sect > 200,]
## look at the data
uniqs = unique(fulldat$dataset)
fulldat$dataset = factor(fulldat$dataset, levels = uniqs[order(nchar(uniqs), uniqs)])

# saveRDS(fulldat,'increments_meta.rds')
# saveRDS(fulldat,'increments_rtcit.rds')

