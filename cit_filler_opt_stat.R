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


# COLLECT DATA ----

setwd(path_neat("results"))
procsv_names = list.files(pattern = "^prolific_export_.*csv$")
# procsv_names = list.files(pattern = "prolific_export_602929f77da1f34a6069612f.csv")

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

    overall_acc_main = aggr_neat(
      dat = subj_itms_base,
      values = valid_trial,
      method = mean,
      filt = (category == 'filler')
    )$aggr_value
    overall_acc_filler = aggr_neat(
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

pids(main_cit_prep$userid[main_cit_prep$condition == 5], filt = dems_pro$userid)


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

neatStats::dems_neat(full_data, group_by = 'filler_type')

# ANALYSIS ----

neatStats::plot_neat(
  full_data,
  values = c(
    'rt_mean_diff_yes',
    'rt_mean_diff_no'
  ), between_vars = 'filler_type'
)

neatStats::anova_neat(
  full_data,
  values = c(
    'rt_mean_diff_yes',
    'rt_mean_diff_no'
  ), between_vars = 'filler_type', bf_added = T
)


t_neat(
  full_data$rt_mean_diff_yes,
  full_data$rt_mean_diff_no,
  pair = T, plots = T, bf_added = T
)

t_neat(
  full_data$rt_mean_diff_yes[full_data$filler_type == 'mixed'],
  full_data$rt_mean_diff_no[full_data$filler_type == 'mixed'],
  pair = T, plots = T, bf_added = T
)

t_neat(
  full_data$rt_mean_diff[full_data$filler_type == 'mixed'],
  full_data$rt_mean_diff[full_data$filler_type == 'verbal'],
  plots = T, bf_added = T
)
t_neat(
  full_data$rt_mean_diff[full_data$filler_type == 'mixed'],
  full_data$rt_mean_diff[full_data$filler_type == 'signal'],
  plots = T, bf_added = T
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
  ), between_vars = 'filler_type'
)

neatStats::plot_neat(
  full_data,
  values = c(
    'rt_mean_diff_yes',
    'rt_mean_diff_no'
  ), between_vars = c('filler_type', "firstcond")
)
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
  ), between_vars = c( "nonverbals"), plot_means = T
)

# SIMS

neatStats::t_neat(
  full_data$rt_mean_diff_no,
  bayestestR::distribution_normal(
    1000,
    mean = 0,
    sd = sd(full_data$rt_mean_diff_no)
  ),
  bf_added = F,
  auc_added = T
)
neatStats::t_neat(
  full_data$rt_mean_diff_yes,
  bayestestR::distribution_normal(
    1000,
    mean = 0,
    sd = sd(full_data$rt_mean_diff_yes)
  ),
  bf_added = F,
  auc_added = T
)

neatStats::t_neat(
  full_data$rt_mean_diff[full_data$filler_type == 'mixed'],
  bayestestR::distribution_normal(
    1000,
    mean = 0,
    sd = sd(full_data$rt_mean_diff[full_data$filler_type == 'mixed'])
  ),
  bf_added = F,
  auc_added = T
)

neatStats::t_neat(
  full_data$rt_mean_diff[full_data$filler_type == 'verbal'],
  bayestestR::distribution_normal(
    1000,
    mean = 0,
    sd = sd(full_data$rt_mean_diff[full_data$filler_type == 'verbal'])
  ),
  bf_added = F,
  auc_added = T
)

neatStats::t_neat(
  full_data$rt_mean_diff[full_data$filler_type == 'signal'],
  bayestestR::distribution_normal(
    1000,
    mean = 0,
    sd = sd(full_data$rt_mean_diff[full_data$filler_type == 'signal'])
  ),
  bf_added = F,
  auc_added = T
)

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
