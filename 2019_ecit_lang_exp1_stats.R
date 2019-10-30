# libs ----

library("neatStats")

# COLLECT DATA ----

setwd(path_neat("lang_results/exp1"))
file_names = list.files(pattern = "^exp_ecit_lang.*txt$")


if ( exists("main_cit_merg") ) {
    rm(main_cit_merg)
    rm(dems_merg)
    rm(all_raw_data)
    rm(exp_unique_names)
}

# exp1_unique_names = exp_unique_names

for(f_name in file_names){
    #f_name = "exp_ecit_lang_1_001_20191021125932.txt"

    print(f_name)

    subj_data = read.table(
        f_name,
        sep = "\t",
        header = TRUE,
        fill = TRUE,
        quote = "\""
    )

    subj_data$stim_type <- as.character(subj_data$stim_type)
    subj_data$stim_type[grepl('^irrelevant', subj_data$stim_type)] = "irrelevant"
    subj_data$stim_type <- factor(subj_data$stim_type)

    subj_itms_base = subj_data[which(
        subj_data$block_number %in% c(4, 5, 6, 7) &
            subj_data$stim_type %in% c("probe", "irrelevant", "targetref", "nontargref", "target")
    ), ]


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

    subj_cond = as.character(subj_itms_base$condition[1])
    subject_number = as.character(subj_itms_base$subject_id[1])

    subj_itms_base$irr_type = 0
    if (subj_cond == 1) {
        subj_itms_base$irr_type[subj_itms_base$block_number %in% c(4, 5)] = 1
    } else {
        subj_itms_base$irr_type[subj_itms_base$block_number %in% c(6, 7)] = 1
    }

    # "pseudo_irr" - 0; "unfam_irr" - 1
    # condition 1: unfam_irr first; condition 2: pseudo_irr first

    subj_acc_rates = neatStats::aggr_neat(
        dat = subj_itms_base,
        values = valid_trial,
        method = mean,
        group_by = c("stim_type", "irr_type"),
        filt = (rt_start >= 150),
        prefix = "acc_rate"
    )

    subj_rt_mean = neatStats::aggr_neat(
        dat = subj_itms_base,
        values = rt_start,
        method = mean,
        group_by = c("stim_type", "irr_type"),
        filt = (rt_start >= 150 & valid_trial == 1),
        prefix = "rt_mean"
    )

    subj_itms_base$press_duration = as.numeric(as.character(subj_itms_base$press_duration))
    subj_dur_mean = neatStats::aggr_neat(
        dat = subj_itms_base,
        values = press_duration,
        method = mean,
        group_by = c("stim_type", "irr_type"),
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

    subject_line = data.frame(subject_id = subject_number,
                              condition = subj_cond,
                              subject_line)

    #### split-half reliability

    probs_sh = subj_itms_base[subj_itms_base$stim_type == "probe" &
                                  subj_itms_base$valid_trial == 1,]
    probs_sh_odd = probs_sh[seq(1, nrow(probs_sh), 2),]
    probs_sh_even = probs_sh[seq(2, nrow(probs_sh), 2),]

    irrs_sh = subj_itms_base[subj_itms_base$stim_type == "irrelevant" &
                                 subj_itms_base$valid_trial == 1,]
    irrs_sh_odd = irrs_sh[seq(1, nrow(irrs_sh), 2),]
    irrs_sh_even = irrs_sh[seq(2, nrow(irrs_sh), 2),]

    subject_line$rt_diff_odd_0 = mean(probs_sh_odd$rt_start[probs_sh_odd$irr_type == 0]) - mean(irrs_sh_odd$rt_start[irrs_sh_odd$irr_type == 0])
    subject_line$rt_diff_even_0 = mean(probs_sh_even$rt_start[probs_sh_even$irr_type == 0]) - mean(irrs_sh_even$rt_start[irrs_sh_even$irr_type == 0])

    subject_line$rt_diff_odd_1 = mean(probs_sh_odd$rt_start[probs_sh_odd$irr_type == 1]) - mean(irrs_sh_odd$rt_start[irrs_sh_odd$irr_type == 1])
    subject_line$rt_diff_even_1 = mean(probs_sh_even$rt_start[probs_sh_even$irr_type == 1]) - mean(irrs_sh_even$rt_start[irrs_sh_even$irr_type == 1])

    ##

    dems_row = subj_data[startsWith(as.character(subj_data$subject_id), 'dems'),]
    dems_heads = c('subject_id', strsplit(as.character(dems_row$subject_id), "/")[[1]][-1])
    dems_dat = c(subject_number, strsplit(as.character(dems_row$condition), "/")[[1]])
    dems = do.call(rbind.data.frame, list(dems_dat))
    colnames(dems) = dems_heads


    if ( exists("main_cit_merg") ) { # add subject aggregations
        main_cit_merg =  merge( main_cit_merg, subject_line, all = T)
        dems_merg = merge( dems_merg, dems, all = T)
    } else {
        main_cit_merg = subject_line
        dems_merg = dems
    }
}

main_cit_merg$rt_mean_diffs_0 = main_cit_merg$rt_mean_probe_0 - main_cit_merg$rt_mean_irrelevant_0
main_cit_merg$dur_mean_diffs_0 = main_cit_merg$dur_mean_probe_0 -
    main_cit_merg$dur_mean_irrelevant_0
main_cit_merg$acc_rate_diffs_0 = main_cit_merg$acc_rate_probe_0 - main_cit_merg$acc_rate_irrelevant_0
main_cit_merg$rt_mean_diffs_1 = main_cit_merg$rt_mean_probe_1 - main_cit_merg$rt_mean_irrelevant_1
main_cit_merg$dur_mean_diffs_1 = main_cit_merg$dur_mean_probe_1 -
    main_cit_merg$dur_mean_irrelevant_1
main_cit_merg$acc_rate_diffs_1 = main_cit_merg$acc_rate_probe_1 - main_cit_merg$acc_rate_irrelevant_1
main_cit_merg$main_overall_acc = ((main_cit_merg$overall_acc_probe) + main_cit_merg$overall_acc_irrelevant * 4) / 5

dems_merg$subject_id = as.character(dems_merg$subject_id)

main_cit_check = merge(x = main_cit_merg, y = dems_merg, by = "subject_id", all = TRUE)

main_cit_data = main_cit_check
main_cit_data = main_cit_data[ main_cit_data$overall_acc_target > 0.5, ]
main_cit_data = main_cit_data[ main_cit_data$overall_acc_targetref > 0.5, ]
main_cit_data = main_cit_data[ main_cit_data$overall_acc_nontargref > 0.5, ]
main_cit_data = main_cit_data[ main_cit_data$main_overall_acc > 0.75, ]

main_cit_check$remaining = ifelse(main_cit_check$subject_id %in% main_cit_data$subject_id,
                                  'remained',
                                  'excluded')

# accuracy exclusion
aggr_neat(
    dat = main_cit_check,
    values = main_overall_acc,
    group_by = c('condition', 'remaining'),
    method = length
)

full_but_lex = main_cit_data[main_cit_data$correct == 4, ]

main_cit_data$remaining = ifelse(main_cit_data$subject_id %in% full_but_lex$subject_id,
                                 'remained',
                                 'excluded')

# probes recognition exclusion
aggr_neat(
    dat = main_cit_data,
    values = main_overall_acc,
    group_by = c('condition', 'remaining'),
    method = length
)

full_but_lex$lextale = as.numeric(as.character(full_but_lex$lextale))
full_data = full_but_lex[full_but_lex$lextale >= 60, ]

full_but_lex$remaining = ifelse(full_but_lex$subject_id %in% full_data$subject_id,
                                 'remained',
                                 'excluded')

# lextale exclusion
aggr_neat(
    dat = full_but_lex,
    values = main_overall_acc,
    group_by = c('condition', 'remaining'),
    method = length
)


# demographics
neatStats::dems_neat(full_data, percent = F)

# ANALYSIS ----

print(":::::::::::::::::::::::CONDITION - pseudo_irr (0) vs. unfam_irr (1) :::::::::::::::::::::")

neatStats::t_neat(full_data$rt_mean_probe_0,
                  full_data$rt_mean_irrelevant_0,
                  pair = T)
neatStats::t_neat(full_data$rt_mean_probe_1,
                  full_data$rt_mean_irrelevant_1,
                  pair = T)

neatStats::t_neat(full_data$acc_rate_probe_0,
                  full_data$acc_rate_irrelevant_0,
                  pair = T)
neatStats::t_neat(full_data$acc_rate_probe_1,
                  full_data$acc_rate_irrelevant_1,
                  pair = T)

neatStats::t_neat(full_data$dur_mean_probe_0,
                  full_data$dur_mean_irrelevant_0,
                  pair = T)
neatStats::t_neat(full_data$dur_mean_probe_1,
                  full_data$dur_mean_irrelevant_1,
                  pair = T)

# within condition differences

neatStats::t_neat(full_data$rt_mean_diffs_0, full_data$rt_mean_diffs_1, pair = T)
neatStats::t_neat(full_data$acc_rate_diffs_0,
                  full_data$acc_rate_diffs_1,
                  pair = T)
neatStats::t_neat(full_data$dur_mean_diffs_0,
                  full_data$dur_mean_diffs_1,
                  pair = T)

# first block between condition differences
# condition 1: unfam_irr first; condition 2: pseudo_irr first
# "pseudo_irr" - 0; "unfam_irr" - 1

neatStats::t_neat(full_data$rt_mean_diffs_0[full_data$condition == 2], full_data$rt_mean_diffs_1[full_data$condition == 1])

# correlation with LexTALE

neatStats::corr_neat(full_data$rt_mean_diffs_0, full_data$lextale)
neatStats::corr_neat(full_data$rt_mean_diffs_1, full_data$lextale)

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
neatStats::t_neat(
    full_data$rt_mean_diffs_1,
    bayestestR::distribution_normal(
        1000,
        mean = 0,
        sd = sd(full_data$rt_mean_diffs_1)*0.5+7
    ),
    bf_added = F,
    auc_added = T
)
neatStats::t_neat(
    full_data$acc_rate_diffs_0,
    bayestestR::distribution_normal(
        1000,
        mean = 0,
        sd = sd(full_data$acc_rate_diffs_0)
    ),
    bf_added = F,
    auc_added = T,
    auc_greater = '2'
)
neatStats::t_neat(
    full_data$acc_rate_diffs_1,
    bayestestR::distribution_normal(
        1000,
        mean = 0,
        sd = sd(full_data$acc_rate_diffs_1)
    ),
    bf_added = F,
    auc_added = T,
    auc_greater = '2'
)
neatStats::t_neat(
    full_data$dur_mean_diffs_0,
    bayestestR::distribution_normal(
        1000,
        mean = 0,
        sd = sd(full_data$dur_mean_diffs_0)
    ),
    bf_added = F,
    auc_added = T,
    auc_greater = '2'
)
neatStats::t_neat(
    full_data$dur_mean_diffs_1,
    bayestestR::distribution_normal(
        1000,
        mean = 0,
        sd = sd(full_data$dur_mean_diffs_1)
    ),
    bf_added = F,
    auc_added = T,
    auc_greater = '2'
)


## logistic regression

log_model(full_data$rt_mean_diffs_0, full_data$dur_mean_diffs_0)
log_model(full_data$rt_mean_diffs_1, full_data$dur_mean_diffs_1)

log_model(full_data$rt_mean_diffs_0, full_data$acc_rate_diffs_0)
log_model(full_data$rt_mean_diffs_1, full_data$acc_rate_diffs_1)


log_model = function(pred1, pred2, n_sim = 1000) {
    cond_real = data.frame(pred1, pred2)
    cond_real$guilt = 1
    colnames(cond_real) = c("p1", "p2", "guilt")

    sim_pred1 = bayestestR::distribution_normal(n_sim,
                                                mean = 0,
                                                sd = sd(pred1))
    sim_pred2 = bayestestR::distribution_normal(n_sim,
                                                mean = 0,
                                                sd = sd(pred2))

    cond_sim = data.frame(sim_pred1, sim_pred2)
    cond_sim$guilt = 0
    colnames(cond_sim) = c("p1", "p2", "guilt")

    cit_data_glm = rbind(cond_real, cond_sim)

    log_regr_base = glm(as.factor(guilt) ~ (p1),
                        data = cit_data_glm,
                        family = "binomial")

    log_regr = glm(as.factor(guilt) ~ (p1) + (p2),
                   data = cit_data_glm,
                   family = "binomial")
    print(summary(log_regr))
    # confint(log_regr, level = .95) # profile likelihood confidence interval
    # confint.default(log_regr, level = .95) #  Wald confidence interval

    cat("\n### BOTH:", fill = TRUE)
    print(wald.test(
        b = coef(log_regr),
        Sigma = vcov(log_regr),
        Terms = 2:3
    ))
    cat("\n### FIRST:", fill = TRUE)
    print(wald.test(
        b = coef(log_regr),
        Sigma = vcov(log_regr),
        Terms = 2
    ))
    cat("\n### SECOND:", fill = TRUE)
    print(wald.test(
        b = coef(log_regr),
        Sigma = vcov(log_regr),
        Terms = 3
    ))
    cat("\n### Compare models:", fill = TRUE)
    print(anova(log_regr_base, log_regr, test = "Chisq"))
    cat("\n### AUC for fitted values:", fill = TRUE)
    cit_data_glm$fitted = log_regr$fitted.values
    # "Logistic predictor"
    t_neat(
        cit_data_glm$fitted[cit_data_glm$guilt == 1],
        cit_data_glm$fitted[cit_data_glm$guilt == 0],
        auc_added = T,
        bf_added = F
    )
}


## split-halves

# "Split-half reliability:"
r_pearson_0 = corr_neat(full_data$rt_diff_odd_0, full_data$rt_diff_even_0)['r']
r_pearson_1 = corr_neat(full_data$rt_diff_odd_1, full_data$rt_diff_even_1)['r']
# Spearman-Brown correction
print((2 * r_pearson_0) / (1 + r_pearson_0))
print((2 * r_pearson_1) / (1 + r_pearson_1))


# SAVE -----

# full_data = subset(full_data, select=-c(subj_coh_d_cit_0,subj_coh_d_cit_1,rep1,rep2,rep3,rep4))

full_data <- full_data[order(as.integer(full_data$subject_id)),]

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
    to_clipboard = F
)

# write.table(main_results, "main_stats_table.txt", quote = F, row.names = F, sep="\t")
