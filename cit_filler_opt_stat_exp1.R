# libs ----

library("neatStats")
lofence = function(numvec) {
  quantile_1st = as.numeric(stats::quantile(numvec, .25, na.rm = TRUE))
  quantile_3rd = as.numeric(stats::quantile(numvec, .75, na.rm = TRUE))
  # return(3 * (quantile_3rd - quantile_1st) + quantile_3rd)
  return(quantile_1st - 3 * (quantile_3rd - quantile_1st))
}
pids = function(idvec, filt) {
  clipr::write_clip(paste(as.character(idvec[idvec %in% filt]), collapse = ","))
}

sim_auc = function(preds) {
  neatStats::t_neat(
    preds,
    bayestestR::distribution_normal(1000,
                                    mean = 0,
                                    sd = sd(preds) * 0.5 + 7),
    bf_added = F,
    auc_added = T
  )
}

# COLLECT DATA ----

setwd(path_neat("results_exp1"))
procsv_names = list.files(pattern = "^prolific_export_.*csv$")
# procsv_names = list.files(pattern = "prolific_export_604dd28241e7742d7168803c.csv")

# allrts = read_dir(
#   '\\.txt$',
#   header = TRUE,
#   fill = TRUE,
#   quote = "\"",
#   filt = !is.na(rt_start) & phase == 'main',
#   stringsAsFactors = FALSE, hush = FALSE
# )

dems_pro = data.frame()
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
                                "Sex",
                                "Student.Status"
                            )]
    dems_pro =  plyr::rbind.fill(dems_pro, dems_table)
}
colnames(dems_pro)[1] = "userid"

# duplicated(dems_pro$userid)

file_names = list.files(pattern = "^fill_opt.*txt$")

# exp1_unique_names = exp_unique_names

for (f_name in enum(file_names)) {
    #f_name = c(0, "fill_opt_17_ROP_20210111163158.txt")

    cat(f_name, '; ')

    subj_data = read.table(
        f_name[2],
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

    subj_data$stim_type[grepl('^irrelevant', subj_data$stim_type)] = "irrelevant"

    subj_itms_base = subj_data[subj_data$phase == 'main', ]
    # subj_itms_base = subj_data[subj_data$phase == 'main' & subj_data$trial_number <= 81, ]

    if (nrow(subj_itms_base) != 648) {
        # just double-check
        # print("number of rows:")
        # print(nrow(subj_itms_base))
        #stop("trial num incorrect: ", nrow(subj_itms_base))
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
      group_by = c("stim_type", "nontargfills"),
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
      group_by = c("stim_type", "nontargfills"),
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
    neatStats::aggr_neat(
      dat = subj_itms_base,
      values = valid_trial,
      method = mean,
      group_by = c("category"),
      prefix = "overall_acc"
    )

    overall_acc_filler = aggr_neat(
      dat = subj_itms_base,
      values = valid_trial,
      method = mean,
      filt = (category == 'filler')
    )$aggr_value
    overall_acc_main = aggr_neat(
      dat = subj_itms_base,
      values = valid_trial,
      method = mean,
      filt = (stim_type %in% c('irrelevant', 'probe'))
    )$aggr_value

    rbind_loop(
      main_cit_merg,
      dems,
      firstcond = subj_itms_base$nontargfills[1],
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



main_cit_prep = merge(main_cit_prep, dems_pro, by = "userid", all = TRUE)

main_cit_prep$probe_correct = as.numeric(as.character(main_cit_prep$probe_correct))


main_cit_data = excl_neat(
  main_cit_prep,
  (
    main_cit_prep$probe_correct > 2
  )
)

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

neatStats::dems_neat(full_data, group_by = 'filler_type', percent = T)


# Initital (opened second part)
# mixed 134+136 = 270
# signal 133+129 = 262
# verbal 129+141 = 270
# neatStats::dems_neat(main_cit_prep, group_by = 'filler_type', percent = T)

# Dropout rates (mixed, signal, verbal)
print((c(270, 262, 270) - c(265,257,259)) / c(270, 262, 270) )


# full_data[is.na(full_data$Sex),]

# conds_done = 25 - table(paste0(full_data$condition, full_data$subcond))
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

facts = c('signal', 'mixed', 'verbal')
full_data$filler_type = factor(full_data$filler_type, levels = facts, labels = facts)

neatStats::plot_neat(
  full_data,
  #eb_method = sd,
  values = c('rt_mean_diff_yes',
             'rt_mean_diff_no'),
  between_vars = 'filler_type',
  value_names = c(
    signal = ' Nonverbal',
    mixed = 'Mixed',
    verbal = 'Verbal',
    rt_mean_diff_yes = 'NT-Fillers included',
    rt_mean_diff_no = 'NT-Fillers removed'
  ), y_title = 'Probe-Control RT Differences'
)

neatStats::anova_neat(
  full_data,
  values = c(
    'rt_mean_diff_yes',
    'rt_mean_diff_no'
  ), between_vars = c('filler_type', 'firstcond'), bf_added = F, plot_means = T
)

neatStats::anova_neat(
  full_data[full_data$filler_type != 'verbal',],
  values = c(
    'rt_mean_diff_yes',
    'rt_mean_diff_no'
  ), between_vars = 'filler_type', bf_added = F
)

 neatStats::anova_neat(
  full_data[full_data$filler_type != 'signal',],
  values = c(
    'rt_mean_diff_yes',
    'rt_mean_diff_no'
  ), between_vars = 'filler_type'
)

#

t_neat(
  full_data$rt_mean_diff_yes[full_data$filler_type == 'mixed'],
  full_data$rt_mean_diff_no[full_data$filler_type == 'mixed'],
  pair = T, plots = T, bf_added = F
)

t_neat(
  full_data$rt_mean_diff[full_data$filler_type == 'mixed'],
  full_data$rt_mean_diff[full_data$filler_type == 'verbal'],
  plots = T, bf_added = T
)
t_neat(
  full_data$rt_mean_diff_yes[full_data$filler_type == 'mixed'],
  full_data$rt_mean_diff_yes[full_data$filler_type == 'verbal'],
  plots = T, bf_added = F
)
# t_neat(
#   full_data$rt_mean_diff_no[full_data$filler_type == 'mixed'],
#   full_data$rt_mean_diff_no[full_data$filler_type == 'verbal'],
#   plots = T, bf_added = T
# )
t_neat(
  full_data$rt_mean_diff[full_data$filler_type == 'mixed'],
  full_data$rt_mean_diff[full_data$filler_type == 'signal'],
  plots = T, bf_added = T#, nonparametric = T
)

t_neat(
  full_data$rt_mean_diff_yes[full_data$filler_type == 'mixed' &
                               full_data$firstcond == 'yes'],
  full_data$rt_mean_diff_no[full_data$filler_type == 'mixed' &
                              full_data$firstcond == 'no'],
  plots = T,
  bf_added = F
)


neatStats::plot_neat(
  full_data,
  values = c(
    'rt_mean_diff_banks',
    'rt_mean_diff_names',
    'rt_mean_diff_nicks',
    'rt_mean_diff_pins'
  )
)

neatStats::plot_neat(
  full_data,
  values = c(
    'rt_mean_diff_banks',
    'rt_mean_diff_names',
    'rt_mean_diff_nicks',
    'rt_mean_diff_pins'
  ), between_vars = 'filler_type', reverse = T, eb_method = sd
)

neatStats::plot_neat(
  full_data,
  values = c(
    'rt_mean_diff_yes',
    'rt_mean_diff_no'
  ), between_vars = c('filler_type', "firstcond"),
  value_names = c(
    signal = ' Nonverbal',
    mixed = 'Mixed',
    verbal = 'Verbal'
  )
)


data_ntfills = full_data[full_data$firstcond == "yes", ]
neatStats::plot_neat(
  data_ntfills,
  values = c(
    'rt_mean_diff_yes'
  ), between_vars = c('filler_type'),
  value_names = c(
    signal = ' Nonverbal',
    mixed = 'Mixed',
    verbal = 'Verbal'
  )
)
t_neat(
  data_ntfills$rt_mean_diff_yes[data_ntfills$filler_type == 'mixed'],
  data_ntfills$rt_mean_diff_yes[data_ntfills$filler_type == 'verbal'],
  bf_added = F
)
t_neat(
  data_ntfills$rt_mean_diff_yes[data_ntfills$filler_type == 'mixed'],
  data_ntfills$rt_mean_diff_yes[data_ntfills$filler_type == 'signal'],
  bf_added = F
)

##
neatStats::plot_neat(
  full_data[full_data$nonverbals != 'none',],
  values = c(
    'rt_mean_diff_yes',
    'rt_mean_diff_no'
  ), between_vars = c( "nonverbals", 'filler_type')
)

neatStats::anova_neat(
  full_data[full_data$filler_type == 'mixed',],
  values = c(
    'rt_mean_diff_yes',
    'rt_mean_diff_no'
  ), between_vars = c( "nonverbals", 'firstcond'), plot_means = T
)

# SIMS

sim_auc(full_data$rt_mean_diff_no)
sim_auc(full_data$rt_mean_diff_yes)

sim_auc(full_data$rt_mean_diff[full_data$filler_type == 'mixed'])
sim_auc(full_data$rt_mean_diff[full_data$filler_type == 'verbal'])
sim_auc(full_data$rt_mean_diff[full_data$filler_type == 'signal'])

sim_auc(full_data$rt_mean_diff_banks)
sim_auc(full_data$rt_mean_diff_names)
sim_auc(full_data$rt_mean_diff_nicks)
sim_auc(full_data$rt_mean_diff_pins)

##### other

rat_data = full_data[full_data$attention == 3 &
                       full_data$r_excitement != 'NA' & full_data$r_anxiety != 'NA',]
rat_data$r_anxiety = as.numeric(as.character(rat_data$r_anxiety))
rat_data$r_realism = as.numeric(as.character(rat_data$r_realism))
rat_data$r_excitement = as.numeric(as.character(rat_data$r_excitement))

peek_neat(rat_data, 'r_anxiety')
peek_neat(rat_data, 'r_realism')
peek_neat(rat_data, 'r_excitement')

corr_neat(rat_data$rt_mean_diff, rat_data$r_anxiety, nonparametric = TRUE)
corr_neat(rat_data$rt_mean_diff, rat_data$r_realism, nonparametric = TRUE)
corr_neat(rat_data$rt_mean_diff, rat_data$r_excitement, nonparametric = TRUE)

peek_neat(rat_data, 'rt_mean_diff', 'r_anxiety')
ggpubr::ggscatter(rat_data, 'r_anxiety', 'rt_mean_diff')


full_data$amount = as.numeric(as.character(full_data$amount))
corr_neat(full_data$rt_mean_diff,full_data$amount)

library('ggplot2')
ggplot2::ggplot(full_data, aes(
  x = amount,
  y = rt_mean_diff, group = filler_type
)) +
  theme_bw() +
  geom_point(shape = 3,
             size = 3, color = "#009900") +
  geom_smooth(
    method = loess,
    fullrange = TRUE,
    level = .95,
    color = "#bb0000",
    fill = "#9999ff",
    size = 0.7
  ) +
  scale_x_log10()

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
  to_clipboard = T,  group_by = c('l1')
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
    aggr_neat(full_data, full_data$acc_rate_irrelevant_normal * 100, round_to = 1),
    aggr_neat(full_data, full_data$acc_rate_target_normal * 100, round_to = 1),
    aggr_neat(full_data, full_data$acc_rate_nontargflr_normal * 100, round_to = 1),
    aggr_neat(full_data, full_data$acc_rate_targetflr_normal * 100, round_to = 1),
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
    aggr_neat(full_data, full_data$acc_rate_irrelevant_scrambled * 100, round_to = 1),
    aggr_neat(full_data, full_data$acc_rate_target_scrambled * 100, round_to = 1),
    aggr_neat(full_data, full_data$acc_rate_nontargflr_scrambled * 100, round_to = 1),
    aggr_neat(full_data, full_data$acc_rate_targetflr_scrambled * 100, round_to = 1),
    aggr_neat(full_data, full_data$acc_rate_diff_scrambled * 100, round_to = 2),

    aggr_neat(full_data, full_data$dur_mean_probe_scrambled, round_to = 0),
    aggr_neat(full_data, full_data$dur_mean_irrelevant_scrambled, round_to = 0),
    aggr_neat(full_data, full_data$dur_mean_target_scrambled, round_to = 0),
    aggr_neat(full_data, full_data$dur_mean_nontargflr_scrambled, round_to = 0),
    aggr_neat(full_data, full_data$dur_mean_targetflr_scrambled, round_to = 0),
    aggr_neat(full_data, full_data$dur_mean_diff_scrambled, round_to = 2)
  ),
  to_clipboard = T, group_by =  c('l1')
)

# write.table(main_results, "main_stats_table.txt", quote = F, row.names = F, sep="\t")
