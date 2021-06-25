library('neatStats')
library("openxlsx")


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
  pval = sum(ls_metric_b >= metricD) / length(ls_metric_b)
  return(as.numeric(pval))
}

nb_corrected = function(data1, data2, n_test_per_train) {
  n = length(data1)
  differences = data1 - data2
  stdev = sd(differences)
  divisor = 1 / n * sum(differences)
  denominator = sqrt(1 / n + n_test_per_train) * stdev
  t_stat = divisor / denominator
  df = n - 1
  p = (1.0 - pt(abs(t_stat), df)) * 2.0
  return(p)
}
nb_corrected_unpair = function(v1, v2, n_test_per_train) {
  m1 = mean(v1)
  m2 = mean(v2)
  n1 = length(v1)
  n2 = length(v2)
  if (n1 != n2) {
    stop('n1 != n2 ', n1, n2)
  }
  s1 = sd(v1)
  s2 = sd(v2)
  t = abs(m1 - m2) / (sqrt((s1 ** 2) + (s2 ^ 2))) / sqrt(1 / n1 + n_test_per_train)
  pval = 2 * pt(t, n1 + n2 - 2, lower.tail = F)
  return(c(t, pval))
}
# Set working directory
setwd(path_neat())

score_dat = readRDS("2021_ml_scores_data.rds")

names(score_dat)

for (sfx in c('_x', '_stud', '_low')) {
  for (m_typ in c('LR', 'LDA', 'ET')) {
    if (!(sfx == 'low' && m_typ == 'LDA')) {
      base_score = score_dat[[paste0('baseline', sfx, '_', m_typ, '_scores')]]
      base_info = score_dat[[paste0('baseline', sfx, '_', m_typ, '_info')]]

      feat_sco_nams = names(score_dat)[grepl(sfx, names(score_dat), fixed = TRUE) &
                                         grepl(m_typ, names(score_dat), fixed = TRUE) &
                                         grepl('feats_', names(score_dat), fixed = TRUE) &
                                         grepl('_scores', names(score_dat), fixed = TRUE)]
      for (feat_s_nam in feat_sco_nams) {
        feat_score = score_dat[[feat_s_nam]]
        feat_info = score_dat[[sub('_scores', '_info', feat_s_nam)]]
        cat('--------------', fill = TRUE)
        cat(base_info['full_title'], ' vs. ', feat_info['full_title'], fill = TRUE)
        b_mean = ro(as.numeric(base_info['s_mean'])*100, round_to = 1)
        f_mean = ro(as.numeric(feat_info['s_mean'])*100, round_to = 1)
        cat(b_mean, ' vs. ', f_mean, fill = TRUE)
        corr_neat(base_score, feat_score, nonparametric = F)
        corr_neat(base_score, feat_score, nonparametric = T)

      }
    }
  }
}







my_t_test(score_dat$feats_low_LR_scores,
             score_dat$baseline_low_LR_scores, 1/5)

nb_corrected(score_dat$feats_low_LR_scores,
       score_dat$baseline_low_LR_scores, 1/5)

## check for BF with correlated data

t_boot(score_dat$feats_low_LR_scores,
       score_dat$baseline_low_LR_scores)
neatStats::t_neat(
  score_dat$feats_low_LR_scores,
  score_dat$baseline_low_LR_scores,
  #bf_added = T,
  nonparametric = TRUE, pair = T
)
neatStats::t_neat(
  score_dat$feats_2_stud_LDA_scores,
  score_dat$baseline_stud_LDA_scores,
  #bf_added = T,
  nonparametric = TRUE, pair = T
)
