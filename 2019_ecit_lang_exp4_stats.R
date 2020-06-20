# libs ----

library("neatStats")
library("plyr")

# COLLECT DATA ----

setwd(path_neat("lang_results/exp4"))
procsv_names = list.files(pattern = "^prolific_export_.*csv$")
if (exists("dems_pro")) {
    rm(dems_pro)
}
for (demname in procsv_names) {
    # demname = "prolific_export_5ea41a55095de0010be51c51.csv"
    print(demname)
    dems_table = read.table(
        demname,
        sep = ",",
        header = TRUE,
        fill = TRUE,
        quote = "\"",
        stringsAsFactors = FALSE
    )
    
    dems_table = dems_table[dems_table$status == "APPROVED" | dems_table$status == "AWAITING REVIEW",
                            c(
                                "participant_id",
                                "time_taken",
                                "age",
                                "prolific_score",
                                "Country.of.Birth",
                                "Current.Country.of.Residence",
                                "Employment.Status",
                                "First.Language",
                                "Fluent.languages",
                                "Nationality",
                                "Sex",
                                "Student.Status"
                            )]
    if (exists("dems_pro")) {
        dems_pro =  merge(dems_pro, dems_table, all = T)
    } else {
        dems_pro = dems_table
    }
}
colnames(dems_pro)[1] = "userid"

# duplicated(dems_pro$userid)

file_names = list.files(pattern = "^lg_exp4_.*txt$")

if (exists("main_cit_merg")) {
    rm(main_cit_merg)
}

# exp1_unique_names = exp_unique_names

for (f_name in file_names) {
    #f_name = "lg_exp4_hu_SOX_20200611204120.txt"
    
    print(f_name)
    
    subj_data = read.table(
        f_name,
        sep = "\t",
        header = TRUE,
        fill = TRUE,
        quote = "\"",
        stringsAsFactors = FALSE
    )
    
    dems_row = subj_data[startsWith(as.character(subj_data$subject_id), 'dems'), ]
    dems_heads = strsplit(dems_row[[2]], "/")[[1]]
    dems_dat = strsplit(dems_row[[3]], "/")[[1]]
    dems = do.call(rbind.data.frame, list(dems_dat))
    colnames(dems) = dems_heads
    
    subj_data$stim_type[grepl('_fill$', subj_data$stim_type)] = "nontargflr"
    subj_data$stim_type[grepl('^irrelevant', subj_data$stim_type)] = "irrelevant"
    
    subj_itms_base = subj_data[subj_data$phase == 'main', ]
    # subj_itms_base = subj_data[subj_data$phase == 'main' & subj_data$trial_number <= 81, ]
    dems$first_lg = subj_data$tested_lang[1]
    
    if (nrow(subj_itms_base) != 648*2) {
        # just double-check
        # print("number of rows:")
        # print(nrow(subj_itms_base))
        stop("trial num incorrect: ", nrow(subj_itms_base))
    }
    
    subj_itms_base$valid_trial = ifelse(
        subj_itms_base$incorrect == 0 &
            subj_itms_base$too_slow == 0 &
            subj_itms_base$rt_start >= 150,
        1,
        0
    )
    
    probs1 = subj_itms_base$rt_start[subj_itms_base$valid_trial == 1 &
                                         subj_itms_base$stim_type == 'probe' &
                                         subj_itms_base$test_num == '1']
    irrs1 = subj_itms_base$rt_start[subj_itms_base$valid_trial == 1 &
                                        subj_itms_base$stim_type == 'irrelevant' &
                                        subj_itms_base$test_num == '1']
    probs2 = subj_itms_base$rt_start[subj_itms_base$valid_trial == 1 &
                                         subj_itms_base$stim_type == 'probe' &
                                         subj_itms_base$test_num == '2']
    irrs2 = subj_itms_base$rt_start[subj_itms_base$valid_trial == 1 &
                                        subj_itms_base$stim_type == 'irrelevant' &
                                        subj_itms_base$test_num == '2']
    dems$dcitph1 = t_neat(probs1, irrs1, bf_added = FALSE, hush = TRUE)$stats['d']
    dems$dcitph2 = t_neat(probs2, irrs2, bf_added = FALSE, hush = TRUE)$stats['d']
    
    subj_acc_rates = neatStats::aggr_neat(
        dat = subj_itms_base,
        values = valid_trial,
        method = mean,
        group_by = c("stim_type", 'tested_lang'),
        filt = (rt_start >= 150),
        prefix = "acc_rate"
    )
    
    subj_acc_rate_blocks = neatStats::aggr_neat(
        dat = subj_itms_base[subj_itms_base$tested_lang == dems$l1,],
        values = valid_trial,
        method = mean,
        group_by = c("stim_type", "block_number"),
        filt = (rt_start >= 150),
        prefix = "acc_B_rate"
    )
    
    subj_rt_mean = neatStats::aggr_neat(
        dat = subj_itms_base,
        values = rt_start,
        method = mean,
        group_by = c("stim_type", 'tested_lang'),
        filt = (rt_start >= 150 & valid_trial == 1),
        prefix = "rt_mean"
    )
    
    subj_rt_mean_blocks = neatStats::aggr_neat(
        dat = subj_itms_base[subj_itms_base$tested_lang == dems$l1,],
        values = rt_start,
        method = mean,
        group_by = c("stim_type", "block_number"),
        filt = (rt_start >= 150 & valid_trial == 1),
        prefix = "rt_B_mean"
    )
    
    subj_rt_mean_fillertype = neatStats::aggr_neat(
        dat = subj_itms_base[subj_itms_base$tested_lang == dems$l1,],
        values = rt_start,
        method = mean,
        group_by = c("stim_type", "fillertype"),
        filt = (rt_start >= 150 & valid_trial == 1),
        prefix = "rt_mean"
    )
    
    subj_itms_base$press_duration = as.numeric(subj_itms_base$press_duration)
    subj_dur_mean = neatStats::aggr_neat(
        dat = subj_itms_base,
        values = press_duration,
        method = mean,
        group_by = c("stim_type", 'tested_lang'),
        filt = (rt_start >= 150 & valid_trial == 1),
        prefix = "dur_mean"
    )
    
    subj_dur_mean_blocks = neatStats::aggr_neat(
        dat = subj_itms_base[subj_itms_base$tested_lang == dems$l1,],
        values = press_duration,
        method = mean,
        group_by = c("stim_type", "block_number"),
        filt = (rt_start >= 150 & valid_trial == 1),
        prefix = "dur_B_mean"
    )
    
    overall_acc = neatStats::aggr_neat(
        dat = subj_itms_base,
        values = valid_trial,
        method = mean,
        group_by = c("stim_type"),
        prefix = "overall_acc"
    )
    
    subject_line = table_neat(
        list(
            subj_acc_rates,
            subj_rt_mean,
            subj_dur_mean,
            subj_acc_rate_blocks,
            subj_rt_mean_blocks,
            subj_dur_mean_blocks,
            subj_rt_mean_fillertype,
            overall_acc
        ),
        transpose = TRUE
    )
    
    blockdata = subj_itms_base[subj_itms_base$rt_start >= 150 &
                                   subj_itms_base$valid_trial == 1 ,]
    blockdata$block_number = as.numeric(blockdata$block_number)
    rt_perb = data.frame(matrix(NA, nrow = 1, ncol = 0))
    counting = 0
    for (maxblock in c(5, 7, 9, 11)) {
        counting = counting + 1
        blocks = blockdata[blockdata$block_number < maxblock,]
        rt_perb[paste0("rt_mean_diff_", counting, "b")] =
            mean(blocks$rt_start[blocks$stim_type == "probe"]) -
            mean(blocks$rt_start[blocks$stim_type == "irrelevant"])
    }
    
    subject_line = cbind(dems, subject_line, rt_perb)
    
    if (exists("main_cit_merg")) {
        # add subject aggregations
        main_cit_merg =  rbind.fill(main_cit_merg, subject_line)
    } else {
        main_cit_merg = subject_line
    }
}

main_cit_prep = main_cit_merg

# i = 0
# for (x in sort(as.character(main_cit_prep$subject_id))) {
#     i = i+1
#     print(i)
#     print(x)
# }

for (colname in names(main_cit_prep)) {
    if (class(main_cit_prep[[colname]]) ==  "numeric" &
        grepl("_probe_", colname, fixed = TRUE)) {
        dat_probe = main_cit_prep[[colname]]
        dat_irrel = main_cit_prep[[sub("_probe_", "_irrelevant_", colname)]]
        newcol = sub("_probe_", "_diff_", colname)
        main_cit_prep[[newcol]] = dat_probe - dat_irrel
        if (grepl("_pl$|_hu$", colname)) {
            l1_col = sub("_pl$|_hu$", "_l1", newcol)
            if (!l1_col %in% colnames(main_cit_prep)) {
                main_cit_prep[[l1_col]] = -1
            }
            main_cit_prep[[l1_col]] = ifelse(
                main_cit_prep$l1 == substr(newcol, nchar(newcol) -
                                               1, nchar(newcol)),
                main_cit_prep[[newcol]],
                main_cit_prep[[l1_col]]
            )
        }
    }
}


main_cit_prep$main_overall_acc = ((main_cit_prep$overall_acc_probe) + main_cit_prep$overall_acc_irrelevant * 4) / 5


# sort(main_cit_prep$main_overall_acc)
# cat(as.character(main_cit_prep$userid[main_cit_prep$l1 == "pl"]), sep = ",", fill = T)
# cat(as.character(main_cit_prep$userid[main_cit_prep$l1 == "hu"]), sep = ",", fill = T)

# cat(as.character(main_cit_prep$userid[main_cit_prep$l1 == "hu" &
#                                           main_cit_prep$bonus == 1]),
#     sep = ",0.5\n",
#     fill = T)
# cat(as.character(main_cit_prep$userid[main_cit_prep$l1 == "hu" &
#                                           main_cit_prep$bonus == 2]),
#     sep = ",1.0\n",
#     fill = T)


main_cit_withdems = merge(main_cit_prep, dems_pro, by = "userid", all = TRUE)

main_cit_withdems$probe_correct1 = as.numeric(as.character(main_cit_withdems$probe_correct1))
main_cit_withdems$probe_correct2 = as.numeric(as.character(main_cit_withdems$probe_correct2))
main_cit_corrprob = main_cit_withdems[(
    main_cit_withdems$probe_correct1 > 2 &
        main_cit_withdems$l1 == main_cit_withdems$first_lg
) |
    (
        main_cit_withdems$probe_correct2 > 2 &
            main_cit_withdems$l1 != main_cit_withdems$first_lg
    ),]


main_cit_withdems$remaining = ifelse(
    main_cit_withdems$subject_id %in% main_cit_corrprob$subject_id,
    'remained',
    'excluded'
)

main_cit_withdems[main_cit_withdems$remaining == 'excluded',]

# accuracy exclusion
aggr_neat(
    dat = main_cit_withdems,
    values = main_overall_acc,
    group_by = c('remaining', 'l1'),
    method = length
)

main_cit_data = main_cit_corrprob

main_cit_data = main_cit_data[main_cit_data$overall_acc_target > 0.4,]
main_cit_data = main_cit_data[main_cit_data$overall_acc_targetflr > 0.4,]
main_cit_data = main_cit_data[main_cit_data$overall_acc_nontargflr > 0.6,]
main_cit_data = main_cit_data[main_cit_data$main_overall_acc > 0.8,]

main_cit_corrprob$remaining = ifelse(main_cit_corrprob$subject_id %in% main_cit_data$subject_id,
                                  'remained',
                                  'excluded')
# accuracy exclusion
aggr_neat(
    dat = main_cit_corrprob,
    values = main_overall_acc,
    group_by = c('remaining', 'l1'),
    method = length
)

full_data = main_cit_data

# demographics

neatStats::dems_neat(full_data, percent = F, group_by = 'l1')
neatStats::dems_neat(full_data, percent = F, group_by = c('alternation'))

# table(full_data$l1, full_data$alternation)

# abs(full_data$dcitph1 - as.numeric(as.character(full_data$dcit1))) < 0.0001


# ANALYSIS ----

t_neat(
    full_data$rt_mean_diff_pl[full_data$l1 == 'pl'],
    full_data$rt_mean_diff_pl[full_data$l1 == 'hu'],
    auc_added = TRUE,
    bf_added = FALSE
)
t_neat(
    full_data$rt_mean_diff_hu[full_data$l1 == 'hu'],
    full_data$rt_mean_diff_hu[full_data$l1 == 'pl'],
    auc_added = TRUE,
    bf_added = FALSE
)

t_neat(
    c(full_data$rt_mean_diff_hu[full_data$l1 == 'hu'],
      full_data$rt_mean_diff_pl[full_data$l1 == 'pl']),
    c(full_data$rt_mean_diff_hu[full_data$l1 == 'pl'],
      full_data$rt_mean_diff_pl[full_data$l1 == 'hu']),
    auc_added = TRUE,
    bf_added = FALSE
)


# etc

neatStats::plot_neat(
    full_data,
    values = c(
        'rt_B_mean_diff_4',
        'rt_B_mean_diff_6',
        'rt_B_mean_diff_8',
        'rt_B_mean_diff_10'
    ),
    between_vars = c(),
    reverse = T
)


neatStats::anova_neat(
    full_data,
    values = c(
        'rt_B_mean_diff_4',
        'rt_B_mean_diff_6',
        'rt_B_mean_diff_8',
        'rt_B_mean_diff_10'
    ),
    between_vars = c('alternation'),
    bf_added = FALSE
)


t_neat(
    full_data$rt_mean_diff_normal[full_data$alternation == 'yes'],
    full_data$rt_mean_diff_scrambled[full_data$alternation == 'yes'],
    pair = TRUE, greater = '2'
)


# correlation with LexTALE

neatStats::corr_neat(full_data$rt_mean_diff, full_data$lextale)

## AUCS

neatStats::t_neat(data_speaker$rt_mean_diffs,
                  data_nonspker$rt_mean_diffs,
                  auc_added = T)


roc1 = neatStats::t_neat(data_speaker$rt_mean_diff_1b,
                  data_nonspker$rt_mean_diff_1b,
                  auc_added = T)$roc_obj
roc2 = neatStats::t_neat(data_speaker$rt_mean_diff_2b,
                  data_nonspker$rt_mean_diff_2b,
                  auc_added = T)$roc_obj
roc3 = neatStats::t_neat(data_speaker$rt_mean_diff_3b,
                  data_nonspker$rt_mean_diff_3b,
                  auc_added = T)$roc_obj
roc4 = neatStats::t_neat(data_speaker$rt_mean_diff_4b,
                  data_nonspker$rt_mean_diff_4b,
                  auc_added = T)$roc_obj

roc_neat(roc1, roc4, greater = "2", pair = T)

neatStats::t_neat(data_speaker$rt_mean_diff_per_t,
                  data_nonspker$rt_mean_diff_per_t,
                  auc_added = T)

neatStats::t_neat(data_speaker$rt_mean_diff_per_fill,
                  data_nonspker$rt_mean_diff_per_fill,
                  auc_added = T)

## SIMULATED AUCS

neatStats::t_neat(
    full_data$rt_mean_diffs,
    bayestestR::distribution_normal(
        1000,
        mean = 0,
        sd = sd(full_data$rt_mean_diffs) * 0.5 + 7
    ),
    bf_added = F,
    auc_added = T
)

    
## split-halves

# "Split-half reliability:"
r_pearson_0 = corr_neat(full_data$rt_diff_odd_0, full_data$rt_diff_even_0)['r']
r_pearson_1 = corr_neat(full_data$rt_diff_odd_1, full_data$rt_diff_even_1)['r']
# Spearman-Brown correction
print((2 * r_pearson_0) / (1 + r_pearson_0))
print((2 * r_pearson_1) / (1 + r_pearson_1))


# SAVE -----

# full_data = subset(full_data, select=-c(subj_coh_d_cit_0,subj_coh_d_cit_1,rep1,rep2,rep3,rep4))

full_data$rt_mean_IrrTarg_diff = full_data$rt_mean_target - full_data$rt_mean_irrelevant
full_data$acc_rate_IrrTarg_diff = full_data$acc_rate_target - full_data$acc_rate_irrelevant
full_data$dur_mean_IrrTarg_diff = full_data$dur_mean_target - full_data$dur_mean_irrelevant

full_data$rt_mean_fillDiff = full_data$rt_mean_targetflr - full_data$rt_mean_nontargflr
full_data$acc_rate_fillDiff = full_data$acc_rate_targetflr - full_data$acc_rate_nontargflr
full_data$dur_mean_fillDiff = full_data$dur_mean_targetflr - full_data$dur_mean_nontargflr

full_data$rt_mean_diff_per_t = full_data$rt_mean_diffs / full_data$rt_mean_IrrTarg_diff
full_data$acc_rate_diff_per_t = full_data$acc_rate_diffs / full_data$acc_rate_IrrTarg_diff
full_data$dur_mean_diff_per_t = full_data$dur_mean_diffs / full_data$dur_mean_IrrTarg_diff

full_data$rt_mean_diff_per_fill = full_data$rt_mean_diffs / full_data$rt_mean_fillDiff
full_data$acc_rate_diff_per_fill = full_data$acc_rate_diffs / full_data$acc_rate_fillDiff
full_data$dur_mean_diff_per_fill = full_data$dur_mean_diffs / full_data$dur_mean_fillDiff


write.table(
    full_data,
    "exp3_all_predictors.txt",
    sep = "\t",
    quote = F,
    row.names = F
)


# write.table(full_data, "results_aggregated_exp2_index_vs_thumb.txt", quote = F, row.names = F, sep="\t")


## final summary ----

main_results = table_neat(
    list(
        aggr_neat(full_data, full_data$rt_mean_probe_0, round_to = 0),
        aggr_neat(full_data, full_data$rt_mean_irrelevant_0, round_to = 0),
        aggr_neat(full_data, full_data$rt_mean_target_0, round_to = 0),
        aggr_neat(full_data, full_data$rt_mean_diffs_0, round_to = 1),
        aggr_neat(full_data, full_data$rt_mean_probe_1, round_to = 0),
        aggr_neat(full_data, full_data$rt_mean_irrelevant_1, round_to = 0),
        aggr_neat(full_data, full_data$rt_mean_target_1, round_to = 0),
        aggr_neat(full_data, full_data$rt_mean_diffs_1, round_to = 1),
        
        aggr_neat(full_data, full_data$acc_rate_probe_0 * 100, round_to = 1),
        aggr_neat(full_data, full_data$acc_rate_irrelevant_0 * 100, round_to = 1),
        aggr_neat(full_data, full_data$acc_rate_target_0 * 100, round_to = 1),
        aggr_neat(full_data, full_data$acc_rate_diffs_0 * 100, round_to = 2),
        aggr_neat(full_data, full_data$acc_rate_probe_1 * 100, round_to = 1),
        aggr_neat(full_data, full_data$acc_rate_irrelevant_1 * 100, round_to = 1),
        aggr_neat(full_data, full_data$acc_rate_target_1 * 100, round_to = 1),
        aggr_neat(full_data, full_data$acc_rate_diffs_1 * 100, round_to = 2),
        
        aggr_neat(full_data, full_data$dur_mean_probe_0, round_to = 0),
        aggr_neat(full_data, full_data$dur_mean_irrelevant_0, round_to = 0),
        aggr_neat(full_data, full_data$dur_mean_target_0, round_to = 0),
        aggr_neat(full_data, full_data$dur_mean_diffs_0, round_to = 1),
        aggr_neat(full_data, full_data$dur_mean_probe_1, round_to = 0),
        aggr_neat(full_data, full_data$dur_mean_irrelevant_1, round_to = 0),
        aggr_neat(full_data, full_data$dur_mean_target_1, round_to = 0),
        aggr_neat(full_data, full_data$dur_mean_diffs_1, round_to = 1)
    ),
    to_clipboard = T
)

main_results = table_neat(
    list(
        aggr_neat(full_data, full_data$acc_rate_probe_0 * 100, round_to = 1),
        aggr_neat(full_data, full_data$acc_rate_irrelevant_0 * 100, round_to = 1),
        aggr_neat(full_data, full_data$acc_rate_nontargflr_0 * 100, round_to = 1),
        aggr_neat(full_data, full_data$acc_rate_target_0 * 100, round_to = 1),
        aggr_neat(full_data, full_data$acc_rate_targetflr_0 * 100, round_to = 1),
        aggr_neat(full_data, full_data$acc_rate_probe_1 * 100, round_to = 1),
        aggr_neat(full_data, full_data$acc_rate_irrelevant_1 * 100, round_to = 1),
        aggr_neat(full_data, full_data$acc_rate_nontargflr_1 * 100, round_to = 1),
        aggr_neat(full_data, full_data$acc_rate_target_1 * 100, round_to = 1),
        aggr_neat(full_data, full_data$acc_rate_targetflr_1 * 100, round_to = 1)
    ),
    to_clipboard = T
)


# write.table(main_results, "main_stats_table.txt", quote = F, row.names = F, sep="\t")
