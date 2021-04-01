# libs ----

library("neatStats")

# COLLECT DATA ----

setwd(path_neat(''))
filler_dat <- readRDS("scit_filler_meta.rds")
filler_dat = filler_dat[!filler_dat$subject_id %in% c('289_CUQ_14976706'), ]

full_merged = data.frame()

for (subj_id in enum(unique(filler_dat$subject_id))) {

    if (sample(1:6, 1) == 1) {
        next
    }

    #subj_id = c(0, "1_UW7740")
    cat(subj_id, "; ")
    subj_data = filler_dat[filler_dat$subject_id == subj_id[2],]

    alltrials = nrow(subj_data)
    allblocks = length(unique(subj_data$block_number))

    subj_data$block = ifelse(
        subj_data$block_number == min(subj_data$block_number),
        'first',
        ifelse(
            subj_data$block_number == max(subj_data$block_number),
            'last',
            'middle'
        )
    )


    subj_data$invalid_trial = ifelse(
        subj_data$incorrect == 0 &
            subj_data$too_slow == 0 &
            subj_data$rt_start >= 150,
        0,
        1
    )

    subj_data$item = subj_data$stim_type
    subj_data$stim_type[startsWith(subj_data$stim_type, 'control')] = 'control'
    overall_er = neatStats::aggr_neat(
        dat = subj_data,
        values = invalid_trial,
        method = mean,
        group_by = c("stim_type"),
        prefix = "overall_er"
    )

    subj_data = subj_data[subj_data$block != 'middle',]

    subj_data_valid = subj_data[subj_data$invalid_trial == 0,]

    subj_rt = neatStats::aggr_neat(
        dat = subj_data_valid,
        values = rt_start,
        method = mean,
        group_by = c("stim_type", "block"),
        prefix = "rt_mean"
    )

    subj_er = neatStats::aggr_neat(
        dat = subj_data,
        values = invalid_trial,
        method = mean,
        group_by = c("stim_type", "block"),
        prefix = "er"
    )

    subj_rt_contrls = neatStats::aggr_neat(
        dat = subj_data_valid[subj_data_valid$stim_type == 'control',],
        values = rt_start,
        method = mean,
        group_by = c("item", "block")
    )
    subj_rt_contrls = subj_rt_contrls[order(subj_rt_contrls$aggr_value),]
    subj_data_valid$item_ordered = subj_data_valid$stim_type
    for (blck in c('first', 'last')) {
        cnt = 0
        for (ctrlnum in subj_rt_contrls$aggr_group[endsWith(subj_rt_contrls$aggr_group, blck)]) {
            cnt = cnt + 1
            subj_data_valid$item_ordered[subj_data_valid$item == substr(ctrlnum, 0, 8) &
                                             subj_data_valid$block == blck] = paste0('control', cnt)
        }
    }

    subj_rt_ordered = neatStats::aggr_neat(
        dat = subj_data_valid[subj_data_valid$stim_type == 'control',],
        values = rt_start,
        method = mean,
        group_by = c("item_ordered", "block")
    )

    rts_per_respnum = c()
    for (myblock in unique(subj_data_valid$block)) {
        mydat1 = subj_data_valid[subj_data_valid$block == myblock &
                                    (
                                        subj_data_valid$stim_type == 'probe' |
                                            startsWith(subj_data_valid$stim_type, 'control')
                                    ), ]
        for (mystim in unique(mydat1$item_ordered)) {
            mydat2 = mydat1[mydat1$item_ordered == mystim, ]
            mydat2$respnum = seq.int(nrow(mydat2))
            for (myrespnum in unique(mydat2$respnum)) {
                mydat3 = mydat2[mydat2$respnum <= myrespnum, ]
                rt_name = paste("rt_mean", myblock, mystim, myrespnum, sep = '_')
                rts_per_respnum[rt_name] = mean(mydat3$rt)
            }
        }
    }

    ## for each control, calculate corresponding probe-control differences per respnum

    rbind_loop(
        main_cit_merg,
        exp = subj_data$exp[1],
        trials = alltrials,
        blocks = allblocks,
        subject_id = subj_data$subject_id[1],
        subj_er,
        subj_rt,
        rts_per_respnum,
        overall_er
    )
}

main_cit_p = main_cit_merg #[, colSums(is.na(main_cit_merg)) == 0]

# saveRDS(main_cit_p, "scit_aggr.rds")

