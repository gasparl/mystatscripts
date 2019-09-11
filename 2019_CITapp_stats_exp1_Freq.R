# libs ----

library("neatStats")
library("TOSTER")
library("plyr")
library("aod")

# COLLECT DATA ----

setwd(path_neat("data"))

file_names_app = list.files(pattern = "^CIT_Mobile_app.*txt$")
file_names_pc = list.files(pattern = "^CIT_Mobile_pc.*txt$")
if (length(file_names_app) != length(file_names_pc)) {
    print(paste(
        "length not OK! app",
        length(file_names_app),
        "vs. pc",
        length(file_names_pc)
    ))
    stop("file number inconsistent")
}

subj_nums = c()
for (filee in file_names_app) {
    subj_nums = c(subj_nums, strsplit(filee, "_")[[1]][4])
}

if (exists("main_cit_merg")) {
    rm(main_cit_merg)
    rm(dems_merg)
    rm(all_raw_data)
    rm(exp_unique_names)
}

for (subject_number in subj_nums) {
    #subject_number = "4"
    #subject_number = "19"
    print(subject_number)
    
    pc_l = list.files(pattern = paste("^CIT_Mobile_pc", subject_number, ".*txt$", sep =
                                          "_"))
    subj_data_pc = read.table(
        pc_l[1] ,
        sep = "\t",
        header = TRUE,
        fill = TRUE,
        quote = "\""
    )
    subj_data_pc$device_status = 0
    
    app_l = list.files(pattern = paste("^CIT_Mobile_app", subject_number, ".*txt$", sep =
                                           "_"))
    subj_data_app = read.table(
        app_l[1] ,
        sep = "\t",
        header = TRUE,
        fill = TRUE,
        quote = "\""
    )
    subj_data_app$device_status = 1
    
    subj_data = merge(subj_data_pc, subj_data_app, all = T)
    
    subj_data$stim_type <- as.character(subj_data$stim_type)
    subj_data$stim_type[grepl('^irrelevant', subj_data$stim_type)] = "irrelevant"
    subj_data$stim_type <- factor(subj_data$stim_type)
    
    subj_itms_base = subj_data[which(
        subj_data$block_number %in% list(4, 5) &
            subj_data$stim_type %in% list(
                "probe",
                "irrelevant",
                "selfrefitem",
                "otherrefitem",
                "target"
            )
    ),]
    
    
    ## filter for names
    
    all_unique_shown = subj_itms_base[!duplicated(subj_itms_base$stimulus_shown) & subj_itms_base$stim_type == "probe", ]
    
    all_unique_shown$category = as.character(all_unique_shown$category)
    probe_vorname = as.character(all_unique_shown$stimulus_shown[startsWith(all_unique_shown$category, "Vorn")])
    probe_nachname = as.character(all_unique_shown$stimulus_shown[startsWith(all_unique_shown$category, "Nachn")])
    
    if (!(probe_vorname %in% shared_names_list ||
          probe_nachname %in% shared_names_list)) {
        print("---SKIPPED")
        next
    } else if (!probe_vorname %in% shared_names_list) {
        subj_itms_base$category = as.character(subj_itms_base$category)
        subj_itms_base = subj_itms_base[startsWith(subj_itms_base$category, "Nachn"),]
        probe_vorname = "-"
        if (nrow(subj_itms_base) != 216) {
            # just double-check
            print("number of rows 1:")
            print(nrow(subj_itms_base))
            stop("trial num incorrect 1")
        }
        all_unique_names = subj_itms_base[!duplicated(subj_itms_base$stimulus_shown),]
        if (nrow(all_unique_names) != 6) {
            # just double-check
            print("number of unique names 1:")
            print(nrow(all_unique_names))
            stop("name problem 1")
        }
    } else if (!probe_nachname %in% shared_names_list) {
        subj_itms_base$category = as.character(subj_itms_base$category)
        subj_itms_base = subj_itms_base[startsWith(subj_itms_base$category, "Vorn"),]
        probe_nachname = "-"
        
        
        if (nrow(subj_itms_base) != 216) {
            # just double-check
            print("number of rows 2:")
            print(nrow(subj_itms_base))
            stop("trial num incorrect 2")
        }
        all_unique_names = subj_itms_base[!duplicated(subj_itms_base$stimulus_shown),]
        if (nrow(all_unique_names) != 6) {
            # just double-check
            print("number of unique names 2:")
            print(nrow(all_unique_names))
            stop("name problem 2")
        }
    } else {
        if (nrow(subj_itms_base) != 432) {
            # just double-check
            print("number of rows 3:")
            print(nrow(subj_itms_base))
            stop("trial num incorrect 3")
        }
        all_unique_names = subj_itms_base[!duplicated(subj_itms_base$stimulus_shown),]
        if (nrow(all_unique_names) != 12) {
            # just double-check
            print("number of unique names 3:")
            print(nrow(all_unique_names))
            stop("name problem 3")
        }
    }
    
    ##
    
    
    
    # if ( exists("exp1_unique_names") ) {
    #     exp1_unique_names =  merge( exp1_unique_names, all_unique_names, all = T)
    # } else {
    #     exp1_unique_names = all_unique_names
    # }
    
    # temp_raw = na.omit(subj_data, cols=c("incorrect"))
    # if ( exists("all_raw_data") ) {
    #     all_raw_data =  merge( all_raw_data, temp_raw, all = T)
    # } else {
    #     all_raw_data = temp_raw
    # }
    # all_raw_data = subset(all_raw_data, select=-c(stimulus_shown,date_in_ms,categ_order))
    # write.table(all_raw_data, "C:/research/proj_citapp/exp1/full_results_exp1_sp_vs_pc.txt", quote = F, row.names = F, sep="\t")
    
    subj_itms_base$valid_trial = ifelse(
        subj_itms_base$incorrect == 0 &
            subj_itms_base$too_slow == 0 &
            subj_itms_base$rt_start >= 150,
        1,
        0
    )
    subj_itms_base <-
        subj_itms_base[order(subj_itms_base$block_number,
                             subj_itms_base$trial_number),]
    subj_cond = as.character(subj_itms_base$device_status[1])
    
    subj_acc_rates = neatStats::aggr_neat(
        dat = subj_itms_base,
        values = valid_trial,
        method = mean,
        group_by = c("stim_type", "device_status"),
        filt = (rt_start >= 150),
        prefix = "acc_rate"
    )
    
    subj_rt_mean = neatStats::aggr_neat(
        dat = subj_itms_base,
        values = rt_start,
        method = mean,
        group_by = c("stim_type", "device_status"),
        filt = (valid_trial == 1),
        prefix = "rt_mean"
    )
    
    subj_itms_base$rt_end[subj_itms_base$rt_end > 1100] = NA
    subj_itms_base$hold_dur = subj_itms_base$rt_end - subj_itms_base$rt_start
    
    subj_dur_mean = neatStats::aggr_neat(
        dat = subj_itms_base,
        values = hold_dur,
        method = mean,
        group_by = c("stim_type", "device_status"),
        filt = (valid_trial == 1),
        prefix = "dur_mean"
    )
    
    corrs_base = ddply(subj_itms_base, c("stim_type", "device_status"), function(x)
        cor(x$rt_start, x$hold_dur, use = "pairwise.complete.obs"))
    
    corrs = data.frame(
        aggr_group = paste("corr", corrs_base$stim_type, corrs_base$device_status, sep = '_'),
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
    
    subject_line$rt_diff_odd_0 = mean(probs_sh_odd$rt_start[probs_sh_odd$device_status == 0]) - mean(irrs_sh_odd$rt_start[irrs_sh_odd$device_status == 0])
    subject_line$rt_diff_even_0 = mean(probs_sh_even$rt_start[probs_sh_even$device_status == 0]) - mean(irrs_sh_even$rt_start[irrs_sh_even$device_status == 0])
    
    subject_line$rt_diff_odd_1 = mean(probs_sh_odd$rt_start[probs_sh_odd$device_status == 1]) - mean(irrs_sh_odd$rt_start[irrs_sh_odd$device_status == 1])
    subject_line$rt_diff_even_1 = mean(probs_sh_even$rt_start[probs_sh_even$device_status == 1]) - mean(irrs_sh_even$rt_start[irrs_sh_even$device_status == 1])
    
    ##
    
    subject_line$vorname = probe_vorname
    subject_line$nachname = probe_nachname
    
    #
    
    dems_l = list.files(pattern = paste(
        "^CIT_Mobile_demographics",
        subject_number,
        ".*txt$",
        sep =
            "_"
    ))
    
    demographics = read.table(dems_l[1],
                              sep = "\t",
                              header = FALSE,
                              fill = TRUE)
    
    colnames(demographics) <-
        c(
            "subject_id",
            "gender",
            "age",
            "rat1",
            "rat2",
            "rat3",
            "rat4",
            "time",
            "hand",
            "country",
            "email"
        )
    
    if (exists("main_cit_merg")) {
        # add subject aggregations
        main_cit_merg =  merge(main_cit_merg, subject_line, all = T)
        dems_merg = merge(dems_merg, demographics, all = T)
    } else {
        main_cit_merg = subject_line
        dems_merg = demographics
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
main_cit_merg = merge(x = main_cit_merg,
                      y = dems_merg,
                      by = "subject_id",
                      all = TRUE)


main_cit_data <-
    main_cit_merg[order(main_cit_merg$condition, main_cit_merg$subject_id), ]
main_cit_data <-
    main_cit_merg[order(main_cit_merg$condition, main_cit_merg$subject_id), ]

main_cit_data = main_cit_data[main_cit_data$overall_acc_target > 0.5,]
main_cit_data = main_cit_data[main_cit_data$main_overall_acc > 0.75,]

main_cit_merg$remaining = ifelse(main_cit_merg$subject_id %in% main_cit_data$subject_id, 1, 0)

excluded_nums = do.call(data.frame,
                        aggregate(main_cit_merg$main_overall_acc, by = list(main_cit_merg$condition, main_cit_merg$remaining), function(x)
                            c(
                                count = length(x),
                                mean = mean(x),
                                sd = sd(x)
                            )))
for (i in 1:nrow(excluded_nums)) {
    row <- excluded_nums[i,]
    print(paste(
        'for Condition:',
        row[1],
        'remaining:',
        row[2],
        'count',
        round(row[3], 2)
    ))
}

full_data_exp1 = main_cit_data


# demographics
neatStats::dems_neat(full_data, percent = F)
#

# ANALYSIS ----


print(
    "::::::::::::::::::::::::::::::CONDITION - PC (0) or PHONE (1)::::::::::::::::::::::::::::::::::::"
)

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

names(full_data)

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

log_model3(full_data$rt_mean_diffs_0, full_data$acc_rate_diffs_0, full_data$dur_mean_diffs_0)
log_model3(full_data$rt_mean_diffs_1, full_data$acc_rate_diffs_1, full_data$dur_mean_diffs_1)

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
    
    log_regr  = glm(as.factor(guilt) ~ (p1) + (p2),
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

log_model3 = function(pred1, pred2, pred3, n_sim = 1000) {
    cond_real = data.frame(pred1, pred2, pred3)
    cond_real$guilt = 1
    colnames(cond_real) = c("p1", "p2", "p3", "guilt")
    
    sim_pred1 = bayestestR::distribution_normal(n_sim,
                                                mean = 0,
                                                sd = sd(pred1))
    sim_pred2 = bayestestR::distribution_normal(n_sim,
                                                mean = 0,
                                                sd = sd(pred2))
    sim_pred3 = bayestestR::distribution_normal(n_sim,
                                                mean = 0,
                                                sd = sd(pred3))
    
    cond_sim = data.frame(sim_pred1, sim_pred2, sim_pred3)
    cond_sim$guilt = 0
    colnames(cond_sim) = c("p1", "p2", "p3", "guilt")
    
    cit_data_glm = rbind(cond_real, cond_sim)
    
    log_regr  = glm(as.factor(guilt) ~ (p1) + (p2) + (p3),
                    data = cit_data_glm,
                    family = "binomial")
    print(summary(log_regr))
    # confint(log_regr, level = .95) # profile likelihood confidence interval
    # confint.default(log_regr, level = .95) #  Wald confidence interval
    
    cat("\nALL:", fill = TRUE)
    print(wald.test(
        b = coef(log_regr),
        Sigma = vcov(log_regr),
        Terms = 2:4
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
    cat("\nTHIRD:", fill = TRUE)
    print(wald.test(
        b = coef(log_regr),
        Sigma = vcov(log_regr),
        Terms = 4
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


corr_neat(full_data$rt_mean_diffs_0,  full_data$dur_mean_diffs_0 )
corr_neat(full_data$rt_mean_diffs_1,  full_data$dur_mean_diffs_1 )

corr_neat(full_data$rt_mean_diffs_0,  full_data$acc_rate_diffs_0 )
corr_neat(full_data$rt_mean_diffs_1,  full_data$acc_rate_diffs_1 )

## ALERTNESS EXTRA

print("alertness per condition:")
for (dvice_stat in list("0", "1")) {
    # 0,1,2,3,4,5
    rt_mean = full_data[[paste("rt_mean_diffs", dvice_stat, sep = "_")]]
    acc = full_data[[paste("acc_rate_diffs", dvice_stat, sep = "_")]]
    hold_rt = full_data[[paste("dur_mean_diffs", dvice_stat, sep = "_")]]
    wakefulness = (full_data$rat1 + full_data$rat3) / 2
    concentration =  (full_data$rat2 + full_data$rat4) / 2
    general_alertness = (wakefulness + concentration) / 2
    print(mean(general_alertness))
    print(sd(general_alertness))
    print(
        "::::::::::::::::::::::::::::::CONDITION - PC (0) or PHONE (1)::::::::::::::::::::::::::::::::::::"
    )
    print(dvice_stat)
    print("###")
    print("RT mean:")
    corr_neat(general_alertness, rt_mean)
    print("Acc:")
    corr_neat(general_alertness, acc)
    print("Hold:")
    corr_neat(general_alertness, hold_rt)
}

corr_neat(full_data$rt_mean_diffs_0,
          full_data$dur_mean_diffs_0)
corr_neat(full_data$rt_mean_diffs_0,  full_data$acc_rate_diffs_0)
corr_neat(full_data$rt_mean_diffs_1,
          full_data$dur_mean_diffs_1)
corr_neat(full_data$rt_mean_diffs_1,  full_data$acc_rate_diffs_1)


## split-halves

# "Split-half reliability:"
r_pearson_0 = corr_neat(full_data$rt_diff_odd_0, full_data$rt_diff_even_0)['r']
r_pearson_1 = corr_neat(full_data$rt_diff_odd_1, full_data$rt_diff_even_1)['r']
# Spearman-Brown correction
print((2 * r_pearson_0) / (1 + r_pearson_0))
print((2 * r_pearson_1) / (1 + r_pearson_1))


# SAVE -----

# full_data = subset(full_data, select=-c(subj_coh_d_cit_0,subj_coh_d_cit_1,time,hand,country,email))

full_data <- full_data[order(as.integer(subject_id)), ]

write.table(
    full_data,
    path_neat("results_aggregated_exp1_sp_vs_pc.txt"),
    quote = F,
    row.names = F,
    sep = "\t"
)



# final summary ----

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
