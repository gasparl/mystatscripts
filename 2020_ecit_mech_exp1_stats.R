# libs ----

library("neatStats")

# COLLECT DATA ----

setwd(path_neat("results/pilot_exp1"))
file_names = list.files(pattern = "^ecit_props_.*txt$")

if ( exists("main_cit_merg") ) {
    rm(main_cit_merg)
}

for(f_name in file_names){
    #f_name = "ecit_props_0_REZ_45596955.txt"

    print(f_name)

    subj_data = read.table(
        f_name,
        sep = "\t",
        header = TRUE,
        fill = TRUE,
        quote = "\"",
        stringsAsFactors = FALSE
    )

    subj_data$stim_type[grepl('^irrelevant', subj_data$stim_type)] = "irrelevant"

    subj_itms_base = subj_data[subj_data$phase == 'main',]


    if (nrow(subj_itms_base) != 648) { # just double-check
        print("number of rows:")
        print(nrow(subj_itms_base))
        stop("trial num incorrect")
    }

    subj_itms_base$valid_trial = ifelse(
        subj_itms_base$incorrect == 0 &
            subj_itms_base$too_slow == 0 &
            subj_itms_base$rt_start >= 150,
        1,
        0
    )

    subj_acc_rates = neatStats::aggr_neat(
        dat = subj_itms_base,
        values = valid_trial,
        method = mean,
        group_by = c("proportion", "lettercase", "stim_type"),
        filt = (rt_start >= 150),
        prefix = "acc_rate"
    )

    subj_rt_mean = neatStats::aggr_neat(
        dat = subj_itms_base,
        values = rt_start,
        method = mean,
        group_by = c("proportion", "lettercase", "stim_type"),
        filt = (rt_start >= 150 & valid_trial == 1),
        prefix = "rt_mean"
    )

    subj_itms_base$press_duration = as.numeric(subj_itms_base$press_duration)
    subj_dur_mean = neatStats::aggr_neat(
        dat = subj_itms_base,
        values = press_duration,
        method = mean,
        group_by = c("proportion", "lettercase", "stim_type"),
        filt = (rt_start >= 150 & valid_trial == 1),
        prefix = "dur_mean"
    )

    overall_acc = neatStats::aggr_neat(
        dat = subj_itms_base,
        values = valid_trial,
        method = mean,
        group_by = c("stim_type"),
        prefix = "overall_acc"
    )

    subject_line = table_neat(list(subj_acc_rates, subj_rt_mean, subj_dur_mean, overall_acc),
                              transpose = TRUE)

    firstc = paste(subj_itms_base$proportion[1], subj_itms_base$lettercase[1], sep = "_")

    firstrt = as.numeric(subject_line[paste0("rt_mean_", firstc, "_probe")]-subject_line[paste0("rt_mean_", firstc, "_irrelevant")])

    subject_line = data.frame(firstcond = firstc,
                              rt_mean_first_diff = firstrt,
                              subject_line)

    ##

    dems_row = subj_data[startsWith(as.character(subj_data$subject_id), 'dems'),]
    dems_heads = strsplit(dems_row[[2]], "/")[[1]]
    dems_dat = strsplit(dems_row[[3]], "/")[[1]]
    dems = do.call(rbind.data.frame, list(dems_dat))
    colnames(dems) = dems_heads
    subject_line = cbind(dems, subject_line)

    if ( exists("main_cit_merg") ) { # add subject aggregations
        main_cit_merg =  merge( main_cit_merg, subject_line, all = T)
    } else {
        main_cit_merg = subject_line
    }
}

main_cit_merg$main_overall_acc = ((main_cit_merg$overall_acc_probe) + main_cit_merg$overall_acc_irrelevant * 4) / 5

main_cit_data = main_cit_merg
main_cit_data = main_cit_data[ main_cit_data$overall_acc_target > 0.5, ]
main_cit_data = main_cit_data[ main_cit_data$overall_acc_targetref > 0.5, ]
main_cit_data = main_cit_data[ main_cit_data$overall_acc_nontargref > 0.5, ]
main_cit_data = main_cit_data[ main_cit_data$main_overall_acc > 0.75, ]

main_cit_merg$remaining = ifelse(main_cit_merg$subject_id %in% main_cit_data$subject_id,
                                  'remained',
                                  'excluded')

# accuracy exclusion
aggr_neat(
    dat = main_cit_merg,
    values = main_overall_acc,
    group_by = c('condition', 'remaining'),
    method = length
)

full_data = main_cit_data[main_cit_data$failed_final == 0, ]

main_cit_data$remaining = ifelse(main_cit_data$subject_id %in% full_data$subject_id,
                                 'remained',
                                 'excluded')
# probes recognition exclusion
aggr_neat(
    dat = main_cit_data,
    values = main_overall_acc,
    group_by = c('condition', 'remaining'),
    method = length
)

# demographics
neatStats::dems_neat(full_data, percent = F)

# setwd(neatStats::path_neat())
# data_exp2 = full_data
# saveRDS(data_exp2, file="data_exp2.Rds")

for (colname in names(full_data)) {
    if (grepl("probe$", colname) & !grepl("overall_acc_", colname)) {
        namebase = sub("probe$", "", colname)
        print(colname)
        print(namebase)
        full_data[[paste0(namebase, "diff")]] = full_data[[colname]] - full_data[[paste0(namebase, "irrelevant")]]
    }
}


# ANALYSIS ----

neatStats::plot_neat(
    full_data,
    values = c('rt_mean_morefam_lowercase_diff', 'rt_mean_morefam_uppercase_diff', 'rt_mean_moreunfam_lowercase_diff', 'rt_mean_moreunfam_uppercase_diff'),
    within_ids = list(prop = c('moreunfam', 'morefam'), case = c('uppercase', 'lowercase'))
)


# firsts

full_data$rt_mean_first_diff
full_data$firstcond

## SIMULATED AUCS

neatStats::t_neat(
    full_data$rt_mean_diffs_0,
    bayestestR::distribution_normal(
        1000,
        mean = 0,
        sd = sd(full_data$rt_mean_diffs_0)*0.5+7
    ),
    bf_added = F,
    auc_added = T
)

# SAVE -----


full_data <- full_data[order(as.integer(full_data$subject_id)),]

# write.table(full_data, "results_aggregated_exp2_index_vs_thumb.txt", quote = F, row.names = F, sep="\t")


## final summary ----



# write.table(main_results, "main_stats_table.txt", quote = F, row.names = F, sep="\t")
