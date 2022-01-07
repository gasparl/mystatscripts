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
                       substring(f_name_rt[2], 1, 23)), file_names_all, value = TRUE)
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
                                     'session_info'),]
  dems_heads = strsplit(dems_row[[2]], "/")[[1]]
  dems_dat = strsplit(dems_row[[3]], "/")[[1]]
  dems_rt = do.call(rbind.data.frame, list(dems_dat))
  colnames(dems_rt) = dems_heads
  dems_rt$subject_id = subj_data_rt$subject_id[1]
  dems_row = subj_data_ar[startsWith(as.character(subj_data_ar$subject_id),
                                     'session_info'),]
  dems_heads = strsplit(dems_row[[2]], "/")[[1]]
  dems_dat = strsplit(dems_row[[3]], "/")[[1]]
  dems_ar = do.call(rbind.data.frame, list(dems_dat))
  colnames(dems_ar) = dems_heads
  dems_ar$subject_id = subj_data_ar$subject_id[1]
  # safety checks
  cond_check = dat_conds[as.character(dat_conds$subject_id) == subj_id, ]
  cond_check$probe_set = substr(cond_check$probe_set, 2, 5)
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

  subj_itms_base = subj_data_rt[subj_data_rt$phase == 'main',]
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

  dems_rt$mean_diff2 = (mean(subj_itms_base$rt_start[subj_itms_base$valid_trial == 1 &
                                                       subj_itms_base$stim_type == 'probe']) -
                          mean(subj_itms_base$rt_start[subj_itms_base$valid_trial == 1 &
                                                         subj_itms_base$stim_type == 'control']))
  if (is.na(dems_rt$mean_diff) || dems_rt$mean_diff == 'NA') {
    warning('mean_diff "NA"')
  } else if (round(dems_rt$mean_diff2, 1) != round(as.numeric(dems_rt$mean_diff), 1)) {
    stop(
      'discrepancy in mean diff: \nmean diff CIT: ',
      dems_rt$mean_diff,
      '\nmean diff R: ',
      dems_rt$mean_diff2
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
  dems$subject_id = NULL
  rbind_loop(
    main_cit_merg,
    subject_id = subj_id,
    dems,
    subj_acc_rates,
    subj_rt_mean,
    correct_counts,
    subj_data_qa)
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

main_cit_data = main_cit_prep

# peek_neat(grp_dat, 'overall_acc_main')
# peek_neat(grp_dat, 'overall_acc_target')
# peek_neat(grp_dat, 'overall_acc_filler')

full_data = main_cit_data # [main_cit_data$subject_id != 'REN_20200905210301',]

# demographics
neatStats::dems_neat(full_data,
                     group_by = c('condition'),
                     percent = T)

# ANALYSIS ----

#full_data = full_data[full_data$batch,]

neatStats::plot_neat(
  full_data,
  #eb_method = sd,
  values = c('rt_mean_diff_varied',
             'rt_mean_diff_regular'),
  #between_vars = c('filler_type'),
  between_vars = c('firstcond'),
  #between_vars = c('filler_type', 'firstcond'),
  #between_vars = c('filler_type'),
  value_names = c(
    regular = 'First: three fillers',
    varied = 'First: six fillers',
    rt_mean_diff_varied = 'Six fillers',
    rt_mean_diff_regular = 'Three fillers'
  ),
  y_title = 'Probe-Control RT Differences'
)

neatStats::anova_neat(
  full_data,
  values = c('rt_mean_diff_varied',
             'rt_mean_diff_regular'),
  between_vars = c('filler_type'),
  bf_added = F,
  plot_means = T
)

t_neat(
  full_data$rt_mean_diff_varied,
  full_data$rt_mean_diff_regular,
  plots = F,
  greater = '1',
  bf_added = T#, nonparametric = T
)

# ratings

corr_neat(rat_data$rt_mean_diff, rat_data$r_anxiety, nonparametric = TRUE)
corr_neat(rat_data$rt_mean_diff, rat_data$r_realism, nonparametric = TRUE)
corr_neat(rat_data$rt_mean_diff, rat_data$r_excitement, nonparametric = TRUE)

full_data$amount = as.numeric(as.character(full_data$amount))
corr_neat(full_data$rt_mean_diff, full_data$amount)

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
  group_by = c('l1')
)


main_results = table_neat(
  list(
    aggr_neat(full_data, full_data$rt_mean_probe_normal, round_to = 0),
    aggr_neat(full_data, full_data$rt_mean_control_normal, round_to = 0),
    aggr_neat(full_data, full_data$rt_mean_target_normal, round_to = 0),
    aggr_neat(full_data, full_data$rt_mean_nontargflr_normal, round_to = 0),
    aggr_neat(full_data, full_data$rt_mean_targetflr_normal, round_to = 0),
    aggr_neat(full_data, full_data$rt_mean_diff_normal, round_to = 1),

    aggr_neat(full_data, full_data$acc_rate_probe_normal * 100, round_to = 1),
    aggr_neat(full_data,
              full_data$acc_rate_control_normal * 100,
              round_to = 1),
    aggr_neat(full_data, full_data$acc_rate_target_normal * 100, round_to = 1),
    aggr_neat(
      full_data,
      full_data$acc_rate_nontargflr_normal * 100,
      round_to = 1
    ),
    aggr_neat(
      full_data,
      full_data$acc_rate_targetflr_normal * 100,
      round_to = 1
    ),
    aggr_neat(full_data, full_data$acc_rate_diff_normal * 100, round_to = 2),

    aggr_neat(full_data, full_data$dur_mean_probe_normal, round_to = 0),
    aggr_neat(full_data, full_data$dur_mean_control_normal, round_to = 0),
    aggr_neat(full_data, full_data$dur_mean_target_normal, round_to = 0),
    aggr_neat(full_data, full_data$dur_mean_nontargflr_normal, round_to = 0),
    aggr_neat(full_data, full_data$dur_mean_targetflr_normal, round_to = 0),
    aggr_neat(full_data, full_data$dur_mean_diff_normal, round_to = 2),

    #

    aggr_neat(full_data, full_data$rt_mean_probe_scrambled, round_to = 0),
    aggr_neat(full_data, full_data$rt_mean_control_scrambled, round_to = 0),
    aggr_neat(full_data, full_data$rt_mean_target_scrambled, round_to = 0),
    aggr_neat(full_data, full_data$rt_mean_nontargflr_scrambled, round_to = 0),
    aggr_neat(full_data, full_data$rt_mean_targetflr_scrambled, round_to = 0),
    aggr_neat(full_data, full_data$rt_mean_diff_scrambled, round_to = 1),

    aggr_neat(full_data, full_data$acc_rate_probe_scrambled * 100, round_to = 1),
    aggr_neat(
      full_data,
      full_data$acc_rate_control_scrambled * 100,
      round_to = 1
    ),
    aggr_neat(
      full_data,
      full_data$acc_rate_target_scrambled * 100,
      round_to = 1
    ),
    aggr_neat(
      full_data,
      full_data$acc_rate_nontargflr_scrambled * 100,
      round_to = 1
    ),
    aggr_neat(
      full_data,
      full_data$acc_rate_targetflr_scrambled * 100,
      round_to = 1
    ),
    aggr_neat(full_data, full_data$acc_rate_diff_scrambled * 100, round_to = 2),

    aggr_neat(full_data, full_data$dur_mean_probe_scrambled, round_to = 0),
    aggr_neat(full_data,
              full_data$dur_mean_control_scrambled,
              round_to = 0),
    aggr_neat(full_data, full_data$dur_mean_target_scrambled, round_to = 0),
    aggr_neat(
      full_data,
      full_data$dur_mean_nontargflr_scrambled,
      round_to = 0
    ),
    aggr_neat(full_data, full_data$dur_mean_targetflr_scrambled, round_to = 0),
    aggr_neat(full_data, full_data$dur_mean_diff_scrambled, round_to = 2)
  ),
  to_clipboard = T,
  group_by =  c('l1')
)

# write.table(main_results, "main_stats_table.txt", quote = F, row.names = F, sep="\t")
