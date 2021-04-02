# libs ----

library("neatStats")

# COLLECT DATA ----

setwd(path_neat(''))
filler_dat <- readRDS("scit_filler_meta.rds")
filler_dat = filler_dat[!filler_dat$subject_id %in% c('289_CUQ_14976706'), ]

full_merged = data.frame()

for (subj_id in enum(unique(filler_dat$subject_id))) {

    # if (sample(1:6, 1) == 1) {
    #     next
    # }

    #subj_id = c(0, "1_UW7740")
    cat(subj_id, "; ")
    subj_data = filler_dat[filler_dat$subject_id == subj_id[2],]

    alltrials = nrow(subj_data)
    b_uniq = unique(subj_data$block_number)
    allblocks = length(b_uniq)
    block_probes = nrow(subj_data[subj_data$block_number == b_uniq[1] &
                                   subj_data$stim_type == 'probe', ])

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
        dat = subj_data_valid[subj_data_valid$stim_type %in% c('control', 'probe'), ],
        values = rt_start,
        method = mean,
        group_by = c("block", "item_ordered"),
        prefix = "rt_mean"
    )
    subj_rt_ordered$aggr_group = paste0(subj_rt_ordered$aggr_group, '_max')

    rts_per_respnum = c()
    max_respnums = list(first = c(), last = c())
    for (myblock in unique(subj_data_valid$block)) {
        mydat1 = subj_data_valid[subj_data_valid$block == myblock &
                                    (
                                        subj_data_valid$stim_type == 'probe' |
                                            startsWith(subj_data_valid$stim_type, 'control')
                                    ), ]
        for (mystim in unique(mydat1$item_ordered)) {
            mydat2 = mydat1[mydat1$item_ordered == mystim, ]
            resp_maxnum = nrow(mydat2)
            max_respnums[[myblock]][paste("max", myblock, mystim, sep = '_')] = resp_maxnum
            mydat2$respnum = seq.int(resp_maxnum)
            for (myrespnum in unique(mydat2$respnum)) {
                mydat3 = mydat2[mydat2$respnum <= myrespnum, ]
                rt_name = paste("rt_mean", myblock, mystim, myrespnum, sep = '_')
                rts_per_respnum[rt_name] = mean(mydat3$rt)
            }
        }
    }

    rt_min_per_respnum = c()
    for (myblock2 in unique(subj_data_valid$block)) {
        for (rnum in c(1:10)) {
            rts5 = c()
            for (myit in c('probe',
                           'control1',
                           'control2',
                           'control3',
                           'control4')) {
                rts5 = c(rts5, rts_per_respnum[paste('rt_mean', myblock2, myit, rnum, sep = '_')])
            }
            if (!anyNA(rts5)) {
                winner = strsplit(names(rts5)[which.min(rts5)], split = '_')[[1]][4]
                rtmin_name = paste("rt_min", myblock2, rnum, sep = '_')
                rt_min_per_respnum[rtmin_name] = winner
            }
        }
    }
    rtfirsts = subj_rt_ordered[startsWith(subj_rt_ordered$aggr_group, "rt_mean_first_"),]
    rtlasts = subj_rt_ordered[startsWith(subj_rt_ordered$aggr_group, "rt_mean_last_"),]
    rt_min_per_respnum['rt_min_first_max'] = strsplit(rtfirsts$aggr_group[which.min(rtfirsts$aggr_value)], split = '_')[[1]][4]
    rt_min_per_respnum['rt_min_last_max'] = strsplit(rtlasts$aggr_group[which.min(rtfirsts$aggr_value)], split = '_')[[1]][4]

    rt_min_per_respnum['rt_MAX_first'] = strsplit(rtfirsts$aggr_group[which.max(rtfirsts$aggr_value)], split = '_')[[1]][4]
    rt_min_per_respnum['rt_MAX_last'] = strsplit(rtlasts$aggr_group[which.max(rtfirsts$aggr_value)], split = '_')[[1]][4]

    rbind_loop(
        main_cit_merg,
        exp = subj_data$exp[1],
        trials = alltrials,
        blocks = allblocks,
        probes = block_probes,
        subject_id = subj_data$subject_id[1],
        subj_er,
        subj_rt,
        subj_rt_ordered,
        rts_per_respnum,
        rt_min_per_respnum,
        c(max_respnums$first, max_respnums$last),
        min_of_maxes_first = min(max_respnums$first),
        min_of_maxes_last = min(max_respnums$last),
        overall_er
    )
}

main_cit_p = main_cit_merg #[, colSums(is.na(main_cit_merg)) == 0]

# saveRDS(main_cit_p, "scit_aggr.rds")

