# libs ----

library("neatStats")

# COLLECT DATA ----

setwd(path_neat("results_labs"))

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

    rbind_loop(
      main_cit_merg,
      dems,
      subj_acc_rates,
      subj_acc_rate_cats,
      subj_acc_rate_nts,
      subj_rt_mean,
      subj_rt_mean_cats,
      subj_rt_mean_nonts,
      overall_acc
    )
}

main_cit_prep = main_cit_merg

for (colname in names(main_cit_prep)) {
    if (class(main_cit_prep[[colname]]) ==  "numeric" &
        grepl("_probe", colname, fixed = TRUE)) {
        dat_probe = main_cit_prep[[colname]]
        dat_irrel = main_cit_prep[[sub("_probe", "_irrelevant", colname)]]
        newcol = sub("_probe", "_diff", colname)
        main_cit_prep[[newcol]] = dat_probe - dat_irrel
    }
}
main_cit_prep$main_overall_acc = ((main_cit_prep$overall_acc_probe) + main_cit_prep$overall_acc_irrelevant * 4) / 5

# full_data = main_cit_prep

main_cit_prep$probe_correct = as.numeric(as.character(main_cit_prep$probe_correct))

main_cit_data = excl_neat(
  main_cit_prep,
  (
    main_cit_prep$probe_correct > 2 |
      main_cit_prep$guilt == 0
  ),
  group_by = 'guilt'
)

main_cit_data_acc = excl_neat(
  main_cit_data,
  overall_acc_target > 0.4 &
    overall_acc_targetflr > 0.4 &
    overall_acc_nontargflr > 0.6 &
    main_overall_acc > 0.8, group_by = 'guilt'
)

full_data = main_cit_data_acc # [main_cit_data_acc$subject_id != 'REN_20200905210301',]

# demographics

neatStats::dems_neat(full_data, group_by = 'guilt')


# ANALYSIS ----

t_neat(
  full_data$rt_mean_diff[full_data$guilt == 1],
  full_data$rt_mean_diff[full_data$guilt == 0],
  auc_added = TRUE,
  bf_added = FALSE, plots = T
)

all = t_neat(
  full_data$rt_mean_diff_yes[full_data$guilt == 1],
  full_data$rt_mean_diff_no[full_data$guilt == 1],
  pair = T, plots = T
)

neatStats::plot_neat(
  full_data,
  values = c(
    'rt_mean_diff_banks',
    'rt_mean_diff_names',
    'rt_mean_diff_nicks',
    'rt_mean_diff_pins'
  ), between_vars = "guilt", reverse = T
)

