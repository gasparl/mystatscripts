library('neatStats')

getrtdiffs = function(datf) {
    datf = subj_itms_base
    datf = datf[datf$stim_type %in% c('probe', 'control'),]
    #datf = datf[datf$stim_type %in% c('probe', 'control', 'target'),]
    prob = datf$rt_start[datf$stim_type == 'probe']
    irr = datf$rt_start[datf$stim_type != 'probe']
    perstim = data.frame(
        type = character(),
        rtmean = numeric(),
        stringsAsFactors = FALSE
    )
    for (thestim in unique(datf$stimulus_shown)) {
        stimdat = datf[datf$stimulus_shown == thestim,]
        perstim[nrow(perstim) + 1,] = c(stimdat$stim_type[1], mean(stimdat$rt))
    }
    perstim$rtmean = as.numeric(perstim$rtmean)
    perstim$scaled = scale(perstim$rt)
    return(c(
        rt_diff = mean(prob) - mean(irr),
        rt_dcit = (mean(prob) - mean(irr)) / sd(irr),
        rt_scaled = mean(perstim$scaled[perstim$type == "probe"])
    ))
}

setwd(path_neat())

preds_lgdat = readRDS("lgcit_trials_both.rds")
preds_lgdat = preds_lgdat[preds_lgdat$valid_trial == 1,]

for (subjectid in enum(unique(preds_lgdat$subject_id))) {
    # subjectid = c(0, "CAN_20200620100940_hu")
    cat(subjectid, '; ')
    subj_itms_base = preds_lgdat[preds_lgdat$subject_id == subjectid[2], ]

    subj_rt_mean = mean(subj_itms_base$rt_start[subj_itms_base$stim_type == 'probe']) -
        mean(subj_itms_base$rt_start[subj_itms_base$stim_type == 'control'])
    subj_rt_mean
    st_blocked_rt_diffs = c()
    st_blocked_rt_dcits = c()
    st_blocked_rt_scaleds = c()
    for (blocknum in unique(subj_itms_base$block_number)) {
        df_block = subj_itms_base[subj_itms_base$block_number == blocknum ,]
        thediffs = getrtdiffs(df_block)
        st_blocked_rt_diffs = c(st_blocked_rt_diffs, thediffs['rt_diff'])
        st_blocked_rt_dcits = c(st_blocked_rt_dcits, thediffs['rt_dcit'])
        st_blocked_rt_scaleds = c(st_blocked_rt_scaleds, thediffs['rt_scaled'])
    }
    rbind_loop(
        main_cit_merg,
        subject_id = subjectid[2],
        study = subj_itms_base$study[1],
        guilt = subj_itms_base$guilt[1],
        rt_diff = subj_rt_mean,
        st_blocked_rt_diff = mean(st_blocked_rt_diffs),
        st_blocked_rt_dcit = mean(st_blocked_rt_dcits),
        st_blocked_rt_scaled = mean(st_blocked_rt_scaleds)
    )
}

fulldat = main_cit_merg

auc_rtdiff = t_neat(fulldat$rt_diff[fulldat$guilt == 'guilty'],
                    fulldat$rt_diff[fulldat$guilt == 'innocent'], auc_added = TRUE)$roc_obj
t_neat(fulldat$st_blocked_rt_diff[fulldat$guilt == 'guilty'],
       fulldat$st_blocked_rt_diff[fulldat$guilt == 'innocent'],
       auc_added = TRUE)
auc_dcit = t_neat(fulldat$st_blocked_rt_dcit[fulldat$guilt == 'guilty'],
                  fulldat$st_blocked_rt_dcit[fulldat$guilt == 'innocent'],
                  auc_added = TRUE)$roc_obj
auc_scaled = t_neat(fulldat$st_blocked_rt_scaled[fulldat$guilt == 'guilty'],
                    fulldat$st_blocked_rt_scaled[fulldat$guilt == 'innocent'],
                    auc_added = TRUE)$roc_obj

roc_neat(auc_dcit, auc_rtdiff, pair = TRUE, plot_rocs = TRUE)
roc_neat(auc_scaled, auc_rtdiff, pair = TRUE, plot_rocs = TRUE)




