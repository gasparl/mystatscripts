# libs ----

library("neatStats")
library("TOSTER")
library("plyr")
library("aod")

# COLLECT DATA ----

setwd(path_neat("data"))
file_names = list.files(pattern = "^CIT_Mobile_app_exp2.*txt$")

if ( exists("main_cit_merg") ) {
    rm(main_cit_merg)
    rm(dems_merg)
    rm(all_raw_data)
    rm(exp_unique_names)
}

# exp1_unique_names = exp_unique_names

for(f_name in file_names){
    #f_name = "CIT_Mobile_app_exp2_1_1_1_1_1547033034378.txt"
 
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
        subj_data$block_number %in% list(4, 5, 7, 8) &
            subj_data$stim_type %in% list("probe", "irrelevant", "selfrefitem", "otherrefitem", "target")
    ), ]
    
    
    if (nrow(subj_itms_base) != 432) { # just double-check
        print("number of rows:")
        print(nrow(subj_itms_base))
        stop("trial num incorrect")
    }


    # all_unique_names = subj_itms_base[!duplicated(subj_itms_base$stimulus_shown), ]
    # if (nrow(all_unique_names) != 12) {
    #     # just double-check
    #     print("number of unique names:")
    #     print(nrow(all_unique_names))
    #     stop("name problem")
    # }
    # 
    # if ( exists("exp2_unique_names") ) {
    #     exp2_unique_names =  merge( exp2_unique_names, all_unique_names, all = T)
    # } else {
    #     exp2_unique_names = all_unique_names
    # }


    # exp1_names = exp1_unique_names[, c("subject_id",
    #                                    "category",
    #                                    "stim_type",
    #                                    "stimulus_shown",
    #                                    "date_in_ms")]
    # exp2_names = exp2_unique_names[, c("subject_id",
    #                                    "category",
    #                                    "stim_type",
    #                                    "stimulus_shown",
    #                                    "date_in_ms")]
    # exp1_names$subject_id = paste0("1_", exp1_names$subject_id)
    # exp2_names$subject_id = paste0("2_", exp2_names$subject_id)
    # final_names = rbind(exp1_names, exp2_names)
    # 
    # all_probes = final_names[final_names$stim_type == "probe",]
    # all_irrs = final_names[final_names$stim_type == "irrelevant",]
    # all_targs_irrs = final_names[final_names$stim_type %in% c("target","irrelevant"),]
    # 
    # length(all_probes$stimulus_shown)
    # length(unique(all_probes$stimulus_shown))
    # 
    # length(all_irrs$stimulus_shown)
    # length(unique(all_irrs$stimulus_shown))
    # 
    # ps_in_irrs = all_probes$stimulus_shown[all_probes$stimulus_shown %in% all_irrs$stimulus_shown]
    # #ps_in_targs_irrs = all_probes$stimulus_shown[all_probes$stimulus_shown %in% all_targs_irrs$stimulus_shown]
    # length(ps_in_irrs)
    # ps_in_irrs_freq = aggregate(data.frame(count = ps_in_irrs), list(value = ps_in_irrs), length)
    # nrow(ps_in_irrs_freq)
    # mean(ps_in_irrs_freq$count)
    # 
    # # length(ps_in_targs_irrs)
    # 
    # irrs_in_ps = all_irrs$stimulus_shown[all_irrs$stimulus_shown %in% all_probes$stimulus_shown]
    # #ps_in_targs_irrs = all_probes$stimulus_shown[all_probes$stimulus_shown %in% all_targs_irrs$stimulus_shown]
    # length(irrs_in_ps)
    # irrs_in_ps_freq = aggregate(data.frame(count = irrs_in_ps), list(value = irrs_in_ps), length)
    # nrow(irrs_in_ps_freq)
    # mean(irrs_in_ps_freq$count)
    

    # all_unique_names = subj_itms_base[!duplicated(subj_itms_base$stimulus_shown), ]
    # all_unique_names = all_unique_names[all_unique_names$stim_type == "probe", ]
    # if ( exists("exp2_unique_names") ) {
    #     exp2_unique_names =  merge( exp2_unique_names, all_unique_names, all = T)
    # } else {
    #     exp2_unique_names = all_unique_names
    # }
    #xx = exp2_unique_names[ , c("subject_id","condition","stimulus_shown", "date_in_ms")] 
    #xx = dcast(setDT( xx ), subject_id ~ rowid(subject_id, prefix = "name"), value.var = c("stimulus_shown","condition","date_in_ms" ) )
    #xx = reshape(xx, idvar=c("subject_id"), direction="wide")
    
    
    
    # temp_raw = na.omit(subj_data, cols=c("incorrect"))
    # if ( exists("all_raw_data") ) {
    #     all_raw_data =  merge( all_raw_data, temp_raw, all = T)
    # } else {
    #     all_raw_data = temp_raw
    # }
    # all_raw_data = subset(all_raw_data, select=-c(stimulus_shown,date_in_ms))
    # write.table(all_raw_data, "C:/research/proj_citapp/exp2/full_results_exp2_index_vs_thumb.txt", quote = F, row.names = F, sep="\t")
    
    subj_itms_base$valid_trial = ifelse(
        subj_itms_base$incorrect == 0 &
            subj_itms_base$too_slow == 0 &
            subj_itms_base$rt_start >= 150,
        1,
        0
    )
    subj_itms_base <-
        subj_itms_base[order(subj_itms_base$block_number, subj_itms_base$trial_number), ]
    subj_cond = as.character(subj_itms_base$condition[1])
    subject_number = as.character(subj_itms_base$subject_id[1])
    
    subj_itms_base$handposition = ifelse(subj_itms_base$handposition == "Zeigefinger", 0, 1)
    # "Zeigefinger" - 0; "Daumen" - 1
    
    
    subj_acc_rates = neatStats::aggr_neat(
        dat = subj_itms_base,
        values = valid_trial,
        method = mean,
        group_by = c("stim_type", "handposition"),
        filt = (rt_start >= 150),
        prefix = "acc_rate"
    )
    
    subj_rt_mean = neatStats::aggr_neat(
        dat = subj_itms_base,
        values = rt_start,
        method = mean,
        group_by = c("stim_type", "handposition"),
        filt = (rt_start >= 150 & valid_trial == 1),
        prefix = "rt_mean"
    )
    
    
    subj_itms_base$rt_end[subj_itms_base$rt_end > 1100] = NA
    subj_itms_base$hold_dur = subj_itms_base$rt_end - subj_itms_base$rt_start
    
    subj_dur_mean = neatStats::aggr_neat(
        dat = subj_itms_base,
        values = hold_dur,
        method = mean,
        group_by = c("stim_type", "handposition"),
        filt = (rt_start >= 150 & valid_trial == 1),
        prefix = "dur_mean"
    )
    
    corrs_base = ddply(subj_itms_base, c("stim_type", "handposition"), function(x)
        cor(x$rt_start, x$hold_dur, use = "pairwise.complete.obs"))
    
    corrs = data.frame(
        aggr_group = paste("corr", corrs_base$stim_type, corrs_base$handposition, sep = '_'),
        aggr_value = corrs_base$V1
    ) 
    
    overall_acc = neatStats::aggr_neat(
        dat = subj_itms_base,
        values = valid_trial,
        method = mean,
        group_by = c("stim_type"),
        prefix = "overall_acc"
    )
    
    subject_line = table_neat(list(subj_acc_rates, subj_rt_mean, subj_dur_mean, overall_acc, corrs),
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
    
    subject_line$rt_diff_odd_0 = mean(probs_sh_odd$rt_start[probs_sh_odd$handposition == 0]) - mean(irrs_sh_odd$rt_start[irrs_sh_odd$handposition == 0])
    subject_line$rt_diff_even_0 = mean(probs_sh_even$rt_start[probs_sh_even$handposition == 0]) - mean(irrs_sh_even$rt_start[irrs_sh_even$handposition == 0])
    
    subject_line$rt_diff_odd_1 = mean(probs_sh_odd$rt_start[probs_sh_odd$handposition == 1]) - mean(irrs_sh_odd$rt_start[irrs_sh_odd$handposition == 1])
    subject_line$rt_diff_even_1 = mean(probs_sh_even$rt_start[probs_sh_even$handposition == 1]) - mean(irrs_sh_even$rt_start[irrs_sh_even$handposition == 1])
    
    ##
    
    dems_string = subj_data[startsWith(as.character(subj_data$subject_id), 'dems'),]$condition
    dems = c(subject_number, strsplit(as.character(dems_string), "/")[[1]])
    if (length(dems) == 9) {
        dems = c(dems, "na", "na")
    }
    dems = do.call(rbind.data.frame, list(dems))
    colnames(dems) = c(
        "subject_id",
        "gender",
        "age",
        "rep1",
        "rep2",
        "rep3",
        "rep4",
        "dcit",
        "version",
        "length",
        "width"
    )
    
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

main_cit_merg = merge(x = main_cit_merg, y = dems_merg, by = "subject_id", all = TRUE)

main_cit_data = main_cit_merg[order(main_cit_merg$condition, main_cit_merg$subject_id),]

main_cit_data = main_cit_data[ main_cit_data$overall_acc_target > 0.5, ]
main_cit_data = main_cit_data[ main_cit_data$main_overall_acc > 0.75, ]

main_cit_merg$remaining = ifelse(main_cit_merg$subject_id %in% main_cit_data$subject_id, 1, 0)

excluded_nums = do.call(data.frame,
                        aggregate(main_cit_merg$main_overall_acc, by = list(main_cit_merg$condition, main_cit_merg$remaining), function(x)
                            c(
                                count = length(x),
                                mean = mean(x),
                                sd = sd(x)
                            )))
for (i in 1:nrow(excluded_nums)) {
    row <- excluded_nums[i, ]
    print(paste(
        'for Condition:',
        row[1],
        'remaining:',
        row[2],
        'count',
        round(row[3], 2)
    ))
}

full_data = main_cit_data

# demographics
neatStats::dems_neat(full_data, percent = F)


# ANALYSIS ----

print("::::::::::::::::::::::::::::::CONDITION - Index (0) or Thumb (1)::::::::::::::::::::::::::::::::::::")

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


neatStats::t_neat(full_data$rt_mean_diffs_0, full_data$rt_mean_diffs_1, pair = T)
neatStats::t_neat(full_data$acc_rate_diffs_0,
                  full_data$acc_rate_diffs_1,
                  pair = T)
neatStats::t_neat(full_data$dur_mean_diffs_0,
                  full_data$dur_mean_diffs_1,
                  pair = T)


dataTOSTpaired(
    data = full_data,
    pairs = list(c(i1 = "rt_mean_diffs_0", i2 = "rt_mean_diffs_1")),
    low_eqbound = -0.4,
    high_eqbound = 0.4,
    alpha = 0.05,
    plots = TRUE
)
neatStats::t_neat(
    full_data$rt_mean_diffs_0,
    full_data$rt_mean_diffs_1,
    pair = T,
    ci = .90
)
dataTOSTpaired(
    data = full_data,
    pairs = list(c(i1 = "acc_rate_diffs_0", i2 = "acc_rate_diffs_1")),
    low_eqbound = -0.4,
    high_eqbound = 0.4,
    alpha = 0.05,
    plots = TRUE
)


neatStats::anova_neat(
    data_per_subject = full_data,
    values = c(
        "dur_mean_probe_0",
        "dur_mean_irrelevant_0",
        "dur_mean_probe_1",
        "dur_mean_irrelevant_1"
    ),
    within_ids = list(
        device = c("_0", "_1"),
        p_vs_i = c("_probe", "_irrel")
    )
)

neatStats::plot_neat(
    data_per_subject = full_data,
    values = c(
        "dur_mean_probe_0",
        "dur_mean_irrelevant_0",
        "dur_mean_probe_1",
        "dur_mean_irrelevant_1"
    ),
    within_ids = list(
        device = c("_0", "_1"),
        p_vs_i = c("_probe", "_irrel")
    ),
    reverse = T
)


## SIMULATED AUCS

neatStats::t_neat(
    full_data$rt_mean_diffs_0,
    bayestestR::distribution_normal(
        1000,
        mean = 0,
        sd = sd(full_data$rt_mean_diffs_0)
    ),
    bf_added = F,
    auc_added = T
)
neatStats::t_neat(
    full_data$rt_mean_diffs_1,
    bayestestR::distribution_normal(
        1000,
        mean = 0,
        sd = sd(full_data$rt_mean_diffs_0)
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
    colnames(cond_real) = c("rt_mean", "dur_mean", "guilt")
    
    sim_rt_mean_diffs_0 = bayestestR::distribution_normal(n_sim,
                                                          mean = 0,
                                                          sd = sd(pred1))
    sim_dur_mean_diffs_0 = bayestestR::distribution_normal(n_sim,
                                                           mean = 0,
                                                           sd = sd(pred2))
    
    cond_sim = data.frame(sim_rt_mean_diffs_0, sim_dur_mean_diffs_0)
    cond_sim$guilt = 0
    colnames(cond_sim) = c("rt_mean", "dur_mean", "guilt")
    
    cit_data_glm = rbind(cond_real, cond_sim)
    
    log_regr  = glm(as.factor(guilt) ~ (rt_mean) + (dur_mean),
                    data = cit_data_glm,
                    family = "binomial")
    print(summary(log_regr))
    # confint(log_regr, level = .95) # profile likelihood confidence interval
    # confint.default(log_regr, level = .95) #  Wald confidence interval
    
    cat("\nBOTH:", fill = TRUE)
    print(wald.test(
        b = coef(log_regr),
        Sigma = vcov(log_regr),
        Terms = 2:3
    ))
    cat("\nFIRST:", fill = TRUE)
    print(wald.test(
        b = coef(log_regr),
        Sigma = vcov(log_regr),
        Terms = 2
    ))
    cat("\nSECOND:", fill = TRUE)
    print(wald.test(
        b = coef(log_regr),
        Sigma = vcov(log_regr),
        Terms = 3
    ))
    cat("", fill = TRUE)
    cit_data_glm$fitted = log_regr$fitted.values
    # "Logistic predictor"
    t_neat(
        cit_data_glm$fitted[cit_data_glm$guilt == 1],
        cit_data_glm$fitted[cit_data_glm$guilt == 0],
        auc_added = T,
        bf_added = F
    )
}


## HOLD-DUR EXTRA

mean(full_data$corr_probe_0)
mean(full_data$corr_probe_1)
mean(full_data$corr_irrelevant_0)
mean(full_data$corr_irrelevant_1)
mean(full_data$corr_target_0)
mean(full_data$corr_target_1)


corr_neat(full_data$rt_mean_diffs_0,  full_data$dur_mean_diffs_0)
corr_neat(full_data$rt_mean_diffs_1,  full_data$dur_mean_diffs_1)

corr_neat(full_data$rt_mean_diffs_0,  full_data$acc_rate_diffs_0)
corr_neat(full_data$rt_mean_diffs_1,  full_data$acc_rate_diffs_1)

## SCREEN SIZE EXTRA

data_screen_size = full_data[!(full_data$length == "na"), ]
data_screen_size$length = as.numeric( as.character( data_screen_size$length ) )
data_screen_size$width = as.numeric( as.character(  data_screen_size$width ) )

corr_neat(data_screen_size$rt_mean_diffs_0,  data_screen_size$width )
corr_neat(data_screen_size$rt_mean_diffs_0,  data_screen_size$length )
corr_neat(data_screen_size$rt_mean_diffs_0,  data_screen_size$width * data_screen_size$length )
corr_neat(data_screen_size$rt_mean_diffs_1,  data_screen_size$width )
corr_neat(data_screen_size$rt_mean_diffs_1,  data_screen_size$length )
corr_neat(data_screen_size$rt_mean_diffs_1,  data_screen_size$width * data_screen_size$length )

corr_neat(data_screen_size$acc_rate_diffs_0,  data_screen_size$width )
corr_neat(data_screen_size$acc_rate_diffs_0,  data_screen_size$length )
corr_neat(data_screen_size$acc_rate_diffs_0,  data_screen_size$width * data_screen_size$length )
corr_neat(data_screen_size$acc_rate_diffs_1,  data_screen_size$width )
corr_neat(data_screen_size$acc_rate_diffs_1,  data_screen_size$length )
corr_neat(data_screen_size$acc_rate_diffs_1,  data_screen_size$width * data_screen_size$length )

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
