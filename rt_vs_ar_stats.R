# libs ----

library("neatStats")

# COLLECT DATA ----

setwd(path_neat("results"))

file_names_rt = list.files(pattern = "^exp_rt_vs_ar_rtcit_.*txt$")
file_names_ar = list.files(pattern = "^exp_rt_vs_ar_arcit_.*txt$")

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
                       substring(f_name_rt[2], 1, 23)), file_names_ar, value = TRUE)
  cat(f_name_rt, '; ')

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
  subj_data$stim_type[grepl('^irrelevant', subj_data$stim_type)] = "irrelevant"

  subj_itms_base = subj_data[subj_data$phase == 'main', ]
  # subj_itms_base = subj_data[subj_data$phase == 'main' & subj_data$trial_number <= 81, ]

  if (nrow(subj_itms_base) != 162 * 4) {
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

  probs1 = subj_itms_base$rt_start[subj_itms_base$valid_trial == 1 &
                                     subj_itms_base$stim_type == 'probe']
  irrs1 = subj_itms_base$rt_start[subj_itms_base$valid_trial == 1 &
                                    subj_itms_base$stim_type == 'irrelevant']
  dems$dcitph = t_neat(probs1, irrs1, bf_added = FALSE, hush = TRUE)$stats['d']

  subj_acc_rates = neatStats::aggr_neat(
    dat = subj_itms_base,
    values = valid_trial,
    method = mean,
    group_by = c("stim_type"),
    filt = (rt_start >= 150),
    prefix = "acc_rate"
  )

  subj_acc_rate_cats = neatStats::aggr_neat(
    dat = subj_itms_base,
    values = valid_trial,
    method = mean,
    group_by = c("stim_type", "category"),
    filt = (rt_start >= 150),
    prefix = "acc_rate"
  )
  subj_acc_rate_nts = neatStats::aggr_neat(
    dat = subj_itms_base,
    values = valid_trial,
    method = mean,
    group_by = c("stim_type", "variety"),
    filt = (rt_start >= 150),
    prefix = "acc_rate"
  )

  subj_rt_mean = neatStats::aggr_neat(
    dat = subj_itms_base,
    values = rt_start,
    method = mean,
    group_by = c("stim_type"),
    filt = (rt_start >= 150 & valid_trial == 1),
    prefix = "rt_mean"
  )

  subj_rt_mean_cats = neatStats::aggr_neat(
    dat = subj_itms_base,
    values = rt_start,
    method = mean,
    group_by = c("stim_type", "category"),
    filt = (rt_start >= 150 & valid_trial == 1),
    prefix = "rt_mean"
  )

  subj_rt_mean_nonts = neatStats::aggr_neat(
    dat = subj_itms_base,
    values = rt_start,
    method = mean,
    group_by = c("stim_type", "variety"),
    filt = (rt_start >= 150 & valid_trial == 1),
    prefix = "rt_mean"
  )

  overall_acc = neatStats::aggr_neat(
    dat = subj_itms_base,
    values = valid_trial,
    method = mean,
    group_by = c("stim_type"),
    prefix = "overall_acc"
  )
  overall_acc_main = aggr_neat(
    dat = subj_itms_base,
    values = valid_trial,
    method = mean,
    filt = (stim_type %in% c('irrelevant', 'probe'))
  )$aggr_value
  overall_acc_filler = aggr_neat(
    dat = subj_itms_base,
    values = valid_trial,
    method = mean,
    filt = (category == 'filler')
  )$aggr_value

  if (as.numeric(substr(dems$subject_id, 5, 12)) < 20210510) {
    pbatch = 'first'
  } else {
    pbatch = 'second'
  }
  rbind_loop(
    main_cit_merg,
    dems,
    batch = pbatch,
    firstcond = subj_itms_base$variety[1],
    subj_acc_rates,
    subj_acc_rate_cats,
    subj_acc_rate_nts,
    subj_rt_mean,
    subj_rt_mean_cats,
    subj_rt_mean_nonts,
    overall_acc,
    overall_acc_main = overall_acc_main,
    overall_acc_filler = overall_acc_filler
  )
}

main_cit_prep = main_cit_merg

# sort(table(strsplit(
#   paste(main_cit_prep$probe_selected, collapse = '|'),
#   split = '|',
#   fixed = TRUE
# )))

#pids(main_cit_prep$userid, filt = dems_pro$userid)


for (colname in names(main_cit_prep)) {
  if (class(main_cit_prep[[colname]]) ==  "numeric" &
      grepl("_probe", colname, fixed = TRUE)) {
    dat_probe = main_cit_prep[[colname]]
    dat_irrel = main_cit_prep[[sub("_probe", "_irrelevant", colname)]]
    newcol = sub("_probe", "_diff", colname)
    main_cit_prep[[newcol]] = dat_probe - dat_irrel
  }
}

main_cit_data = main_cit_prep

# peek_neat(grp_dat, 'overall_acc_main')
# peek_neat(grp_dat, 'overall_acc_target')
# peek_neat(grp_dat, 'overall_acc_filler')

for (grp in unique(main_cit_data$filler_type)) {
  cat(grp, fill = TRUE)
  grp_dat = main_cit_data[main_cit_data$filler_type == grp, ]
  main_cit_data = excl_neat(
    main_cit_data,
    (
      overall_acc_main >= lofence(grp_dat$overall_acc_main) &
        overall_acc_target >= lofence(grp_dat$overall_acc_target) &
        overall_acc_filler >= lofence(grp_dat$overall_acc_filler)
    ) | main_cit_data$filler_type != grp
  )
}

full_data = main_cit_data # [main_cit_data$subject_id != 'REN_20200905210301',]

# demographics
full_data$gender[full_data$gender == 'NA'] = NA
neatStats::dems_neat(full_data,
                     group_by = c('condition'),
                     percent = T)
#neatStats::dems_neat(full_data, group_by = c('subcond'), percent = T)
neatStats::dems_neat(full_data, group_by = 'filler_type', percent = T)


# Initital (opened second part)
# varied 160+157 = 317
# regular 158+149 = 307
# neatStats::dems_neat(main_cit_prep, group_by = 'filler_type', percent = T)

# Dropout rates (varied, regular)
# print((c(317, 307) - c(305,294)) / c(317, 307) )


# full_data[is.na(full_data$Sex),]

# conds_done = 37 - table(paste0(full_data$condition, full_data$subcond))
# condlist = c()
# for (fullcond in names(conds_done)) {
#   numbs = conds_done[fullcond]
#   if (numbs < 0) {
#     numbs = 0
#   }
#   condlist = c(condlist, rep(fullcond, numbs))
# }
# table(condlist)
# clipr::write_clip(paste(condlist, collapse = "','"))

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

# first blocks
t_neat(
  full_data$rt_mean_diff_varied[full_data$firstcond == 'varied'],
  full_data$rt_mean_diff_regular[full_data$firstcond == 'regular'],
  plots = F,
  greater = '1',
  bf_added = T#, nonparametric = T
)
# second blocks
t_neat(
  full_data$rt_mean_diff_varied[full_data$firstcond == 'regular'],
  full_data$rt_mean_diff_regular[full_data$firstcond == 'varied'],
  plots = F,
  greater = '1',
  bf_added = T#, nonparametric = T
)


t_neat(
  full_data$rt_mean_diff[full_data$filler_type == 'generic'],
  full_data$rt_mean_diff[full_data$filler_type == 'specific'],
  plots = F,
  bf_added = T#, nonparametric = T
)

# var_tests(full_data$rt_mean_diff, full_data$filler_type)

t_neat(
  full_data$rt_mean_diff_varied[full_data$filler_type == 'regular3'],
  full_data$rt_mean_diff_regular[full_data$filler_type == 'regular3'],
  plots = F,
  greater = '1',
  pair = T,
  bf_added = T#, nonparametric = T
)

t_neat(
  full_data$rt_mean_diff_varied[full_data$filler_type == 'varied'],
  full_data$rt_mean_diff_regular[full_data$filler_type == 'varied'],
  plots = F,
  greater = '1',
  pair = T,
  bf_added = T#, nonparametric = T
)

# ORDER


full_data$rt_mean_diff_1st = ifelse(
  full_data$firstcond == 'varied',
  full_data$rt_mean_diff_varied,
  full_data$rt_mean_diff_regular
)
full_data$rt_mean_diff_2nd = ifelse(
  full_data$firstcond == 'regular',
  full_data$rt_mean_diff_regular,
  full_data$rt_mean_diff_varied
)


neatStats::plot_neat(
  full_data,
  #eb_method = sd,
  values = c('rt_mean_diff_1st',
             'rt_mean_diff_2nd'),
  between_vars = c('firstcond'),
  #between_vars = c('filler_type', 'firstcond'),
  value_names = c(
    rt_mean_diff_varied = 'Six fillers',
    rt_mean_diff_regular = 'Three fillers'
  ),
  y_title = 'Probe-Control RT Differences'
)


# numbers vs. arrows

t_neat(
  full_data$rt_mean_diff_regular[full_data$nonverbals == 'numbers'],
  full_data$rt_mean_diff_regular[full_data$nonverbals == 'arrows'],
  plots = F,
  #greater = '2',
  bf_added = T#, nonparametric = T
)

t_neat(
  full_data$rt_mean_diff_regular[full_data$nonverbals == 'numbers' &
                                   full_data$filler_type == 'varied' &
                                   full_data$firstcond != 'varied'],
  full_data$rt_mean_diff_regular[full_data$nonverbals == 'arrows' &
                                   full_data$filler_type == 'varied' &
                                   full_data$firstcond != 'varied'],
  plots = F,
  greater = '1',
  bf_added = T#, nonparametric = T
)


# SIMS

sim_auc(full_data$rt_mean_diff_varied)
sim_auc(full_data$rt_mean_diff_regular)

sim_auc(full_data$rt_mean_diff[full_data$filler_type == 'varied'])
sim_auc(full_data$rt_mean_diff[full_data$filler_type == 'regular3'])

sim_auc(full_data$rt_mean_diff[full_data$filler_type == 'varied' &
                                 full_data$firstcond != 'varied'])
sim_auc(full_data$rt_mean_diff[full_data$filler_type == 'regular3' &
                                 full_data$firstcond != 'varied'])



sim_auc(full_data$rt_mean_diff_banks)
sim_auc(full_data$rt_mean_diff_names)
sim_auc(full_data$rt_mean_diff_nicks)
sim_auc(full_data$rt_mean_diff_pins)



rat_data = full_data[full_data$attention == 3 &
                       full_data$r_excitement != 'NA' &
                       full_data$r_anxiety != 'NA' &
                       full_data$r_realism != 'NA', ]
rat_data$r_anxiety = as.numeric(as.character(rat_data$r_anxiety))
rat_data$r_realism = as.numeric(as.character(rat_data$r_realism))
rat_data$r_excitement = as.numeric(as.character(rat_data$r_excitement))

peek_neat(rat_data, 'r_anxiety')
peek_neat(rat_data, 'r_realism')
peek_neat(rat_data, 'r_excitement')

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
    aggr_neat(full_data, full_data$rt_mean_irrelevant, round_to = 0),
    aggr_neat(full_data, full_data$rt_mean_target, round_to = 0),
    aggr_neat(full_data, full_data$rt_mean_nontargflr, round_to = 0),
    aggr_neat(full_data, full_data$rt_mean_targetflr, round_to = 0),
    aggr_neat(full_data, full_data$rt_mean_diff, round_to = 1),

    aggr_neat(full_data, full_data$acc_rate_probe * 100, round_to = 1),
    aggr_neat(full_data, full_data$acc_rate_irrelevant * 100, round_to = 1),
    aggr_neat(full_data, full_data$acc_rate_target * 100, round_to = 1),
    aggr_neat(full_data, full_data$acc_rate_nontargflr * 100, round_to = 1),
    aggr_neat(full_data, full_data$acc_rate_targetflr * 100, round_to = 1),
    aggr_neat(full_data, full_data$acc_rate_diff * 100, round_to = 2),

    aggr_neat(full_data, full_data$dur_mean_probe, round_to = 0),
    aggr_neat(full_data, full_data$dur_mean_irrelevant, round_to = 0),
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
    aggr_neat(full_data, full_data$rt_mean_irrelevant_normal, round_to = 0),
    aggr_neat(full_data, full_data$rt_mean_target_normal, round_to = 0),
    aggr_neat(full_data, full_data$rt_mean_nontargflr_normal, round_to = 0),
    aggr_neat(full_data, full_data$rt_mean_targetflr_normal, round_to = 0),
    aggr_neat(full_data, full_data$rt_mean_diff_normal, round_to = 1),

    aggr_neat(full_data, full_data$acc_rate_probe_normal * 100, round_to = 1),
    aggr_neat(
      full_data,
      full_data$acc_rate_irrelevant_normal * 100,
      round_to = 1
    ),
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
    aggr_neat(full_data, full_data$dur_mean_irrelevant_normal, round_to = 0),
    aggr_neat(full_data, full_data$dur_mean_target_normal, round_to = 0),
    aggr_neat(full_data, full_data$dur_mean_nontargflr_normal, round_to = 0),
    aggr_neat(full_data, full_data$dur_mean_targetflr_normal, round_to = 0),
    aggr_neat(full_data, full_data$dur_mean_diff_normal, round_to = 2),

    #

    aggr_neat(full_data, full_data$rt_mean_probe_scrambled, round_to = 0),
    aggr_neat(full_data, full_data$rt_mean_irrelevant_scrambled, round_to = 0),
    aggr_neat(full_data, full_data$rt_mean_target_scrambled, round_to = 0),
    aggr_neat(full_data, full_data$rt_mean_nontargflr_scrambled, round_to = 0),
    aggr_neat(full_data, full_data$rt_mean_targetflr_scrambled, round_to = 0),
    aggr_neat(full_data, full_data$rt_mean_diff_scrambled, round_to = 1),

    aggr_neat(full_data, full_data$acc_rate_probe_scrambled * 100, round_to = 1),
    aggr_neat(
      full_data,
      full_data$acc_rate_irrelevant_scrambled * 100,
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
    aggr_neat(
      full_data,
      full_data$dur_mean_irrelevant_scrambled,
      round_to = 0
    ),
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
