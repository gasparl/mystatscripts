# libs ----

library("neatStats")

# COLLECT DATA ----

setwd(path_neat("results"))

file_names_rt = list.files(pattern = "^exp_rt_vs_ar_rtcit_.*txt$")
file_names_all = list.files(pattern = "^exp_rt_vs_ar_.*txt$")

dat_conds = read.table(
  'exp_rt_vs_ar_table.txt',
  sep = "\t",
  header = TRUE,
  fill = TRUE,
  quote = "\"",
  stringsAsFactors = FALSE
)

for (f_name_rt in enum(file_names_rt)) {
  # f_name_rt = c(0, "exp_rt_vs_ar_rtcit_8_20220106_1446.txt")

  f_name_ar = grep(sub('_rtcit_', '_arcit_',
                       substring(f_name_rt[2], 1, 23)),
                   file_names_all,
                   value = TRUE)
  f_name_qa = grep(sub('_rtcit_', '_qa_',
                       substring(f_name_rt[2], 1, 23)), file_names_all, value = TRUE)
  cat(f_name_rt, f_name_ar, f_name_qa, '; ')
  if (strsplit(f_name_rt[2], "_")[[1]][7] != strsplit(f_name_ar, "_")[[1]][7] |
      strsplit(f_name_ar, "_")[[1]][7] != strsplit(f_name_qa, "_")[[1]][7]) {
    stop("dates don't match!")
  }

  subj_data_rt = read.table(
    f_name_rt[2],
    sep = "\t",
    header = TRUE,
    fill = TRUE,
    quote = "\"",
    stringsAsFactors = FALSE
  )

  subj_data_ar = read.table(
    f_name_ar,
    sep = "\t",
    header = TRUE,
    fill = TRUE,
    quote = "\"",
    stringsAsFactors = FALSE
  )

  subj_data_qa = read.table(
    f_name_qa,
    sep = "\t",
    header = TRUE,
    fill = TRUE,
    quote = "\"",
    stringsAsFactors = FALSE
  )

  subj_id = subj_data_rt$subject_id[1]
  # session info
  dems_row = subj_data_rt[startsWith(as.character(subj_data_rt$subject_id),
                                     'session_info'), ]
  dems_heads = strsplit(dems_row[[2]], "/")[[1]]
  dems_dat = strsplit(dems_row[[3]], "/")[[1]]
  dems_rt = do.call(rbind.data.frame, list(dems_dat))
  colnames(dems_rt) = dems_heads
  dems_rt$subject_id = subj_data_rt$subject_id[1]
  dems_row = subj_data_ar[startsWith(as.character(subj_data_ar$subject_id),
                                     'session_info'), ]
  dems_heads = strsplit(dems_row[[2]], "/")[[1]]
  dems_dat = strsplit(dems_row[[3]], "/")[[1]]
  dems_ar = do.call(rbind.data.frame, list(dems_dat))
  colnames(dems_ar) = dems_heads
  dems_ar$subject_id = subj_data_ar$subject_id[1]
  # safety checks
  cond_check = dat_conds[as.character(dat_conds$subject_id) == subj_id,]
  cond_check$probe_set = sub('p', '', cond_check$probe_set, fixed = TRUE)
  for (info_name in c("subject_id",
                      "guilt",
                      "cit_order",
                      "set_order",
                      "block_order",
                      "probe_set")) {
    if (dems_rt[[info_name]] != dems_ar[[info_name]]) {
      stop('discrepancy in ',
           info_name,
           '! RT-CIT: ',
           dems_rt[[info_name]],
           '; AR-CIT: ',
           dems_ar[[info_name]])
    } else if (dems_rt[[info_name]] != subj_data_qa[[info_name]]) {
      stop('discrepancy in ',
           info_name,
           '! RT-CIT: ',
           dems_rt[[info_name]],
           '; QA: ',
           subj_data_qa[[info_name]])
    } else if (dems_rt[[info_name]] != cond_check[[info_name]]) {
      stop('discrepancy in ',
           info_name,
           '! RT-CIT: ',
           dems_rt[[info_name]],
           '; table: ',
           cond_check[[info_name]])
    }
  }
  probes_rt = paste(unique(subj_data_rt$stimulus_shown[subj_data_rt$stim_type == 'probe']), collapse = "; ")
  probes_ar = paste(unique(subj_data_ar$stimulus_shown[subj_data_ar$stim_type == 'probe']), collapse = "; ")
  if (cond_check$probes_rt != probes_rt ||
      cond_check$probes_ans != probes_ar) {
    stop(
      'discrepancy in probes: \nTABLE AR: ',
      probes_ar,
      '\nAR-CIT: ',
      cond_check$probes_ans,
      '\nTABLE RT: ',
      cond_check$probes_rt,
      '\nRT-CIT: ',
      probes_rt
    )
  }
  ## RT-CIT

  subj_itms_base = subj_data_rt[subj_data_rt$phase == 'main', ]
  # subj_itms_base = subj_data_rt[subj_data_rt$phase == 'main' & subj_data_rt$trial_number <= 81, ]

  if (nrow(subj_itms_base) != 162 * 2) {
    # just double-check
    print("number of rows:")
    print(nrow(subj_itms_base))
    stop("trial num incorrect: ", nrow(subj_itms_base))
  }

  subj_itms_base$valid_trial = ifelse(
    subj_itms_base$incorrect == 0 &
      subj_itms_base$too_slow == 0 &
      subj_itms_base$rt_start >= 150,
    1,
    0
  )

  dems_rt$rt_mean_diff = (mean(subj_itms_base$rt_start[subj_itms_base$valid_trial == 1 &
                                                       subj_itms_base$stim_type == 'probe']) -
                          mean(subj_itms_base$rt_start[subj_itms_base$valid_trial == 1 &
                                                         subj_itms_base$stim_type == 'control']))
  if (is.na(dems_rt$mean_diff) || dems_rt$mean_diff == 'NA') {
    warning('mean_diff "NA"')
  } else if (round(dems_rt$rt_mean_diff, 1) != round(as.numeric(dems_rt$mean_diff), 1)) {
    stop(
      'discrepancy in mean diff: \nmean diff CIT: ',
      dems_rt$mean_diff,
      '\nmean diff R: ',
      dems_rt$rt_mean_diff
    )
  }

  subj_acc_rates = neatStats::aggr_neat(
    dat = subj_itms_base,
    values = valid_trial,
    method = mean,
    group_by = c("stim_type", "category"),
    filt = (rt_start >= 150),
    prefix = "acc_rate"
  )

  subj_rt_mean = neatStats::aggr_neat(
    dat = subj_itms_base,
    values = rt_start,
    method = mean,
    group_by = c("stim_type", "category"),
    filt = (valid_trial == 1),
    prefix = "rt_mean"
  )

  correct_counts = neatStats::aggr_neat(
    dat = subj_itms_base,
    values = valid_trial,
    method = length,
    group_by = c("stim_type", "category"),
    filt = (valid_trial == 1 & category != 'inducer'),
    prefix = "correct_num"
  )

  dems = merge(dems_rt, dems_ar)
  dems = merge(dems, subj_data_qa)
  dems$subject_id = NULL
  rbind_loop(main_cit_merg,
             subject_id = subj_id,
             dems,
             subj_acc_rates,
             subj_rt_mean,
             correct_counts)
}

main_cit_prep = main_cit_merg

for (colname in names(main_cit_prep)) {
  if (class(main_cit_prep[[colname]]) ==  "numeric" &
      grepl("_probe", colname, fixed = TRUE)) {
    dat_probe = main_cit_prep[[colname]]
    dat_irrel = main_cit_prep[[sub("_probe", "_control", colname)]]
    newcol = sub("_probe", "_diff", colname)
    main_cit_prep[[newcol]] = dat_probe - dat_irrel
  }
}

main_cit_data = excl_neat(main_cit_prep, filt = (correct_noted == 4 |
                                                   guilt == 'innocent'))
main_cit_data = excl_neat(main_cit_data,
                          filt = (correct_num_probe_banks > 2 |
                                    correct_num_probe_names > 2))
main_cit_data = excl_neat(main_cit_data,
                          filt = (correct_num_control_banks > 5 |
                                    correct_num_control_names > 5))

# peek_neat(grp_dat, 'overall_acc_main')

full_data = main_cit_data # [main_cit_data$subject_id != 'REN_20200905210301',]

# demographics
neatStats::dems_neat(full_data,
                     group_by = c('guilt', 'cit_order'),
                     percent = T)

# ANALYSIS ----

neatStats::plot_neat(
  full_data,
  #eb_method = sd,
  values = c('rt_mean_diff_banks',
             'rt_mean_diff_names'),
  #between_vars = c('guilt', 'cit_order'),
  between_vars = c('guilt'),
  y_title = 'Probe-Control RT Differences'
)

t_neat(
  full_data$rt_mean_probe_names,
  full_data$rt_mean_control_names,
  plots = F,
  greater = '1' #, nonparametric = T
)
t_neat(
  full_data$rt_mean_probe_banks,
  full_data$rt_mean_control_banks,
  plots = F,
  greater = '1' #, nonparametric = T
)

# ratings

t_neat(
  full_data$accuracy1,
  full_data$accuracy2,
  plots = F, nonparametric = T
)
t_neat(
  full_data$detected1,
  full_data$detected2,
  plots = F, nonparametric = T
)

corr_neat(full_data$mean_diff, full_data$accuracy1, nonparametric = TRUE)
corr_neat(full_data$mean_diff, full_data$accuracy2, nonparametric = TRUE)
corr_neat(full_data$mean_diff, full_data$detected1, nonparametric = TRUE)
corr_neat(full_data$mean_diff, full_data$detected2, nonparametric = TRUE)
corr_neat(full_data$mean_diff, full_data$attention, nonparametric = TRUE)
corr_neat(full_data$mean_diff, full_data$realism, nonparametric = TRUE)
corr_neat(full_data$mean_diff, full_data$anxiety, nonparametric = TRUE)
corr_neat(full_data$mean_diff, full_data$excitement, nonparametric = TRUE)
corr_neat(full_data$mean_diff, full_data$attention, nonparametric = TRUE)

# write.table(full_data, "results_aggregated_exp2_index_vs_thumb.txt", quote = F, row.names = F, sep="\t")

## final summary ----

main_results = table_neat(
  list(
    aggr_neat(full_data, full_data$rt_mean_probe, round_to = 0),
    aggr_neat(full_data, full_data$rt_mean_control, round_to = 0),
    aggr_neat(full_data, full_data$rt_mean_target, round_to = 0),
    aggr_neat(full_data, full_data$rt_mean_nontargflr, round_to = 0),
    aggr_neat(full_data, full_data$rt_mean_targetflr, round_to = 0),
    aggr_neat(full_data, full_data$rt_mean_diff, round_to = 1),

    aggr_neat(full_data, full_data$acc_rate_probe * 100, round_to = 1),
    aggr_neat(full_data, full_data$acc_rate_control * 100, round_to = 1),
    aggr_neat(full_data, full_data$acc_rate_target * 100, round_to = 1),
    aggr_neat(full_data, full_data$acc_rate_nontargflr * 100, round_to = 1),
    aggr_neat(full_data, full_data$acc_rate_targetflr * 100, round_to = 1),
    aggr_neat(full_data, full_data$acc_rate_diff * 100, round_to = 2),

    aggr_neat(full_data, full_data$dur_mean_probe, round_to = 0),
    aggr_neat(full_data, full_data$dur_mean_control, round_to = 0),
    aggr_neat(full_data, full_data$dur_mean_target, round_to = 0),
    aggr_neat(full_data, full_data$dur_mean_nontargflr, round_to = 0),
    aggr_neat(full_data, full_data$dur_mean_targetflr, round_to = 0),
    aggr_neat(full_data, full_data$dur_mean_diff, round_to = 2)
  ),
  to_clipboard = T,
  group_by = c('guilt')
)

# write.table(main_results, "main_stats_table.txt", quote = F, row.names = F, sep="\t")
