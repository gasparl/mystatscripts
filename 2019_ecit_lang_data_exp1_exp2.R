library('neatStats')
library('plyr')
library('bayestestR')
setwd(path_neat())

exp1 = readRDS("data_exp1.Rds")
exp2 = readRDS("data_exp2.Rds")
exp1$exp = "1"
exp2$exp = "2"

for (colname in names(exp2)) {
    if (!colname %in%  names(exp1)) {
        print(colname)
        exp2[colname] = NULL
    }
}

all_data = rbind(exp1, exp2)

# cat(names(all_data), fill = TRUE, sep = "','")

to_remove = c('overall_acc_irrelevant','overall_acc_nontargref','overall_acc_probe','overall_acc_target','overall_acc_targetref','main_overall_acc','country','hand','reps1','rep2','rep3','drtn','dcit','lextale','correct','w1234',"rt_diff_odd_0","rt_diff_even_0","rt_diff_odd_1","rt_diff_even_1")

all_data[to_remove] <- list(NULL)

simrownum = nrow(all_data)

simed_data = data.frame(subject_id = paste0(1:simrownum, "sim"))

for (colname in names(all_data)) {
    oldvals = all_data[[colname]]
    if (class(oldvals) ==  "numeric") {
        if (grepl("_diffs_", colname, fixed = TRUE)) {
            simmean = 0
        } else if (grepl("_probe_", colname, fixed = TRUE)) {
            colname = "rt_mean_probe_1"
            oldvals = all_data[[sub("_probe_", "_irrelevant_", colname)]]
            simmean = mean(oldvals)
        } else {
            simmean = mean(oldvals)
        }
        vsim = bayestestR::distribution_normal(simrownum,
                                               mean = simmean,
                                               sd = sd(oldvals))
        simed_data[colname] = sample(vsim)
    }
}
simed_data$exp = "sim"

all_predictors = rbind.fill(all_data, simed_data)

all_predictors$rt_mean_IrrTarg_diff_0 = all_predictors$rt_mean_target_0 - all_predictors$rt_mean_irrelevant_0
all_predictors$rt_mean_IrrTarg_diff_1 = all_predictors$rt_mean_target_1 - all_predictors$rt_mean_irrelevant_1
all_predictors$acc_rate_IrrTarg_diff_0 = all_predictors$acc_rate_target_0 - all_predictors$acc_rate_irrelevant_0
all_predictors$acc_rate_IrrTarg_diff_1 = all_predictors$acc_rate_target_1 - all_predictors$acc_rate_irrelevant_1
all_predictors$dur_mean_IrrTarg_diff_0 = all_predictors$dur_mean_target_0 - all_predictors$dur_mean_irrelevant_0
all_predictors$dur_mean_IrrTarg_diff_1 = all_predictors$dur_mean_target_1 - all_predictors$dur_mean_irrelevant_1

all_predictors$rt_mean_fillDiff_0 = all_predictors$rt_mean_targetref_0 - all_predictors$rt_mean_nontargref_0
all_predictors$rt_mean_fillDiff_1 = all_predictors$rt_mean_targetref_1 - all_predictors$rt_mean_nontargref_1
all_predictors$acc_rate_fillDiff_0 = all_predictors$acc_rate_targetref_0 - all_predictors$acc_rate_nontargref_0
all_predictors$acc_rate_fillDiff_1 = all_predictors$acc_rate_targetref_0 - all_predictors$acc_rate_nontargref_0
all_predictors$dur_mean_fillDiff_0 = all_predictors$dur_mean_targetref_0 - all_predictors$dur_mean_nontargref_0
all_predictors$dur_mean_fillDiff_1 = all_predictors$dur_mean_targetref_1 - all_predictors$dur_mean_nontargref_1


# the idea is that these differences reflect the probe-irrelevant difference, so if e.g. targ-irr difference is low, then even a low probe-irr difference will indicate guilt

# so perhaps alternative predictor could be divided by these correlated variables, e.g. (PvsI / TvsI)


all_predictors$outcome = ifelse(all_predictors$exp == "sim", 0, 1)

names(all_predictors)

simfunc(all_predictors$rt_mean_diffs_0[all_predictors$outcome == "1"])

write.table(
    all_predictors,
    "all_predictors.txt",
    sep = "\t",
    quote = F,
    row.names = F
)


# check correlations
realdata = all_predictors[all_predictors$exp != "sim",]
realdata$exp
corr_neat(realdata$rt_mean_diffs_0, realdata$rt_mean_IrrTarg_diff_0)
corr_neat(realdata$rt_mean_diffs_1, realdata$rt_mean_IrrTarg_diff_1)
corr_neat(realdata$rt_mean_diffs_0, realdata$rt_mean_fillDiff_0)
corr_neat(realdata$rt_mean_diffs_1, realdata$rt_mean_fillDiff_1)

corr_neat(realdata$acc_rate_diffs_0, realdata$acc_rate_IrrTarg_diff_0)
corr_neat(realdata$acc_rate_diffs_1, realdata$acc_rate_IrrTarg_diff_1)
corr_neat(realdata$acc_rate_diffs_0, realdata$acc_rate_fillDiff_0)
corr_neat(realdata$acc_rate_diffs_1, realdata$acc_rate_fillDiff_1)

corr_neat(realdata$dur_mean_diffs_0, realdata$dur_mean_IrrTarg_diff_0)
corr_neat(realdata$dur_mean_diffs_1, realdata$dur_mean_IrrTarg_diff_1)
corr_neat(realdata$dur_mean_diffs_0, realdata$dur_mean_fillDiff_0)
corr_neat(realdata$dur_mean_diffs_1, realdata$dur_mean_fillDiff_1)
