library('neatStats')
library("openxlsx")

# Set working directory
setwd(path_neat())

r_dict = list(
  baseline_x = c('report_20200729_cla_1', 'baseline (1a)'),
  # 'rt_mean_diff'

  feats_x_2 = c('report_20200729_cla_2', 'model-based (2 features)'),
  # "rt_mean_diff", "acc_rate_diff"

  feats_x_5 = c('report_20200729_cla_3', 'model-based (5 features)'),
  # "rt_mean_diff", "rt_mean_ti_diff", "rt_mean_probe", "rt_mean_irrelevant",  "rt_mean_target"

  feats_x_12 = c('report_20200729_cla_4', 'model-based (12 features)'),
  # "rt_mean_diff", "acc_rate_diff", "rt_mean_ti_diff", "acc_rate_ti_diff", "rt_mean_probe", "rt_mean_irrelevant", "rt_mean_target", "acc_rate_probe",  "acc_rate_irrelevant", "acc_rate_target", 'gender', 'age'

  feats_x_6sign = c('report_20200731_cla_1', 'model-based (6 sign. features)'),
  # "rt_mean_diff", "acc_rate_diff", "acc_rate_ti_diff", "rt_mean_probe", "rt_mean_irrelevant", 'age'

  baseline_stud = c('report_20200803_cla_1', 'baseline (1b)'),
  # "rt_mean_diff", 'study'

  feats_2_stud = c('report_20200803_cla_2', 'model-based (2 features + exp.)'),
  # "rt_mean_diff", "acc_rate_diff", 'study'

  feats_5_stud = c('report_20200803_cla_3', 'model-based (5 features + exp.)'),
  # "rt_mean_diff", "rt_mean_ti_diff", "rt_mean_probe", "rt_mean_irrelevant",  "rt_mean_target", 'study'

  feats_12_stud = c('report_20200803_cla_4', 'model-based (12 features + exp.)'),
  # "rt_mean_diff", "acc_rate_diff", "rt_mean_ti_diff", "acc_rate_ti_diff", "rt_mean_probe", "rt_mean_irrelevant", "rt_mean_target", "acc_rate_probe",  "acc_rate_irrelevant", "acc_rate_target", 'gender', 'age', 'study'

  feats_6sign_stud = c('report_20200805_cla_1', 'model-based (6 sign. features + exp.)'),
  # "rt_mean_diff", "acc_rate_diff", "acc_rate_ti_diff", "rt_mean_probe", "acc_rate_probe", "acc_rate_target", 'age', 'study'

  #### 2021

  baseline_low = c('20210422_cla_2', 'baseline (2)'),
  # 'rt_mean_diff'

  feats_low = c('20210422_cla_1', 'model-based (11 lower-level features)')
  # 'age', 'rt_mean_probe', 'rt_mean_irrelevant', 'rt_mean_target', 'rt_sd_probe', 'rt_sd_irrelevant', 'rt_sd_target', 'acc_rate_probe', 'acc_rate_irrelevant', 'acc_rate_target', 'gender'

)

scores_data = list()

for (key in names(r_dict)) {
  names = r_dict[[key]]
  if (startsWith(names[1], 'report_2020')) {
    filenames = list(c(
      paste0(
        names[1],
        '/DV_outcome/m1_LDA/LDA_clf_scores_outcome.xlsx'
      ),
      'LDA'
    ),
    c(
      paste0(
        names[1],
        '/DV_outcome/m2_LR/LOGREG_clf_scores_outcome.xlsx'
      ),
      'LR'
    ),
    c(
      paste0(names[1], '/DV_outcome/m5_ET/ET_clf_scores_outcome.xlsx'),
      'ET'
    ))
  } else {
    filenames = list(c(
      paste0(
        names[1],
        '/res_clf_ET/plots_outcome/ET_scores_outcome.xlsx'
      ),
      'ET'
    ),
    c(
      paste0(
        names[1],
        '/res_clf_LR/plots_outcome/LR_scores_outcome.xlsx'
      ),
      'LR'
    ))
  }
  for (fname in filenames) {
    sheetdat = read.xlsx(fname[1], sheet = 1)
    print('-----------')
    ## add in a list/df:
    print(c(key, names[1], names[2], fname[1], fname[2], mean(as.numeric(sheetdat$ACC))))
    #
    newkey_score = paste(key, fname[2], 'scores', sep = '_')
    newkey_info = paste(key, fname[2], 'info', sep = '_')
    scores_data[[newkey_score]] = as.numeric(sheetdat$ACC)
    scores_data[[newkey_info]] = c(
      full_title = paste(fname[2], names[2], sep = ' '),
      s_mean = mean(as.numeric(sheetdat$ACC)),
      s_file = names[1],
      s_title = names[2],
      s_model = fname[2]
    )
  }
}

saveRDS(scores_data, "2021_ml_scores_data.rds")

