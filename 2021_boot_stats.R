library('neatStats')
library("openxlsx")

mean = median

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

ggpubr::ggdensity(score_dat$feats_low_LR_scores)

quantile(score_dat$feats_low_LR_scores, probs = c(0, 0.025, 0.5, 0.975, 1))

'Returns p-value for 0-hypothesis that there is no predictiable pattern
    between X and Y. p-value is the fraction of values that are more extreme
    after shuffling the targets than the values obtained without shuffling.
    p_val = (C + 1) / (n_permutations + 1) with C = n_permutation >= no permut
    Ref: Ojala and Garriga. Permutation Tests for Studying Classifier
    Performance. The Journal of Machine Learning Research (2010) vol. 11'

