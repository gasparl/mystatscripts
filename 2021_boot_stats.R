library('neatStats')
t_boot = function(var1, var2, n_rep = 20000) {
  # 100,000 bootstrap samples; Efron, 1992
  # first should be greater
  metricA = mean(var1)
  metricB = mean(var2)
  metricD = metricA - metricB
  metricT = mean(c(var1, var2))
  dataN = c(var1 - mean(var1) + metricT, var2 - mean(var2) + metricT)
  ls_metric_b = c()
  for (i in 1:n_rep) {
    ls_metric_b = c(ls_metric_b, mean(sample(dataN, length(var1), replace = TRUE)) -
                      mean(sample(dataN, length(var2), replace = TRUE)))
  }
  pval = sum(ls_metric_b >= metricD)/length(ls_metric_b)
  return(pval)
}

# Set working directory
setwd(path_neat())

# pred_data = readRDS("all_predictors_meta2021.rds")

r_dict =list(
    preds2 = c('20210422_cla_1', 'Model-based (11 lower-level features)'),
    # 'age', 'rt_mean_probe', 'rt_mean_irrelevant', 'rt_mean_target', 'rt_sd_probe', 'rt_sd_irrelevant', 'rt_sd_target', 'acc_rate_probe', 'acc_rate_irrelevant', 'acc_rate_target', 'gender'

    baseline_2 = c('20210422_cla_2', 'Baseline (2)'),
    # 'rt_mean_diff'

    baseline_1 = c('report_20200729_cla_1', 'Baseline (1a)'),
    # 'rt_mean_diff'

    c('report_20200729_cla_2', 'Model-based (2 features)'),
    # "rt_mean_diff", "acc_rate_diff"

    c('report_20200729_cla_3', 'Model-based (5 features)'),
    # "rt_mean_diff", "rt_mean_ti_diff", "rt_mean_probe", "rt_mean_irrelevant",  "rt_mean_target"


    c('report_20200729_cla_4', 'Model-based (12 features)'),
    # "rt_mean_diff", "acc_rate_diff", "rt_mean_ti_diff", "acc_rate_ti_diff", "rt_mean_probe", "rt_mean_irrelevant", "rt_mean_target", "acc_rate_probe",  "acc_rate_irrelevant", "acc_rate_target", 'gender', 'age'

    c('report_20200731_cla_1', 'Model-based (6 sign. features)'),
    # "rt_mean_diff", "acc_rate_diff", "acc_rate_ti_diff", "rt_mean_probe", "rt_mean_irrelevant", 'age'

    c('report_20200803_cla_1', 'Baseline (1b)'),
    # "rt_mean_diff", 'study'

    c('report_20200803_cla_2', 'Model-based (2 features + exp.)'),
    # "rt_mean_diff", "acc_rate_diff", 'study'

    c('report_20200803_cla_3', 'Model-based (5 features + exp.)'),
    # "rt_mean_diff", "rt_mean_ti_diff", "rt_mean_probe", "rt_mean_irrelevant",  "rt_mean_target", 'study'

    c('report_20200803_cla_4', 'Model-based (12 features + exp.)'),
    # "rt_mean_diff", "acc_rate_diff", "rt_mean_ti_diff", "acc_rate_ti_diff", "rt_mean_probe", "rt_mean_irrelevant", "rt_mean_target", "acc_rate_probe",  "acc_rate_irrelevant", "acc_rate_target", 'gender', 'age', 'study'

    c('report_20200805_cla_1', 'Model-based (6 sign. features + exp.)')
    # "rt_mean_diff", "acc_rate_diff", "acc_rate_ti_diff", "rt_mean_probe", "acc_rate_probe", "acc_rate_target", 'age', 'study'


  )