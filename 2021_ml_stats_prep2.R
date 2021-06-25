library('neatStats')
library('boot')

set.seed(100)

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
  return(as.numeric(p))
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
  return(as.numeric(pval))
}
# Set working directory
setwd(path_neat())

score_dat = readRDS("2021_ml_scores_data.rds")

names(score_dat)

results_merged = data.frame()
for (sfx in c('_x', '_stud', '_low')) {
  for (m_typ in c('LR', 'LDA', 'ET')) {
    if (!(sfx == '_low' && m_typ == 'LDA')) {
      base_score = score_dat[[paste0('baseline', sfx, '_', m_typ, '_scores')]]
      base_info = score_dat[[paste0('baseline', sfx, '_', m_typ, '_info')]]
      booted_ci = boot.ci(boot.out = boot(base_score,
                                          function(vec, idx) {
                                            mean(vec[idx])
                                          }, R = 10000),
                          type = "bca")$bca
      newline = c(
        mean = mean(base_score),
        setNames(as.numeric(booted_ci)[4:5], c('b_low', 'b_upp')),
        mean_ci(base_score, distance_only = FALSE)
      )
      newline = data.frame(measure = base_info['full_title'],
                           method = m_typ,
                           as.list(newline))
      results_merged = plyr::rbind.fill(results_merged, newline)
      if (sfx == 'low') {
        test_p_train = 1 / 4
      } else {
        test_p_train = 1 / 9
      }
      feat_sco_nams = names(score_dat)[grepl(sfx, names(score_dat), fixed = TRUE) &
                                         grepl(m_typ, names(score_dat), fixed = TRUE) &
                                         grepl('feats_', names(score_dat), fixed = TRUE) &
                                         grepl('_scores', names(score_dat), fixed = TRUE)]
      for (feat_s_nam in feat_sco_nams) {
        feat_score = score_dat[[feat_s_nam]]
        feat_info = score_dat[[sub('_scores', '_info', feat_s_nam)]]
        cat('--------------', fill = TRUE)
        cat(base_info['full_title'], ' vs. ', feat_info['full_title'], fill = TRUE)
        b_mean = ro(as.numeric(base_info['s_mean']) * 100, round_to = 1)
        f_mean = ro(as.numeric(feat_info['s_mean']) * 100, round_to = 1)
        cat(b_mean, ' vs. ', f_mean, fill = TRUE)
        tstat = t_neat(base_score,
                       feat_score,
                       bf_added = TRUE,
                       nonparametric = TRUE)$stats

        booted_ci = boot.ci(boot.out = boot(feat_score,
                                            function(vec, idx) {
                                              mean(vec[idx])
                                            }, R = 10000),
                            type = "bca")$bca
        newline = c(
          mean = mean(feat_score),
          setNames(as.numeric(booted_ci)[4:5], c('b_low', 'b_upp')),
          mean_ci(feat_score, distance_only = FALSE),
          p_wilcox = as.numeric(tstat['p']),
          p_boot = t_boot(feat_score, base_score),
          p_nb = nb_corrected_unpair(base_score, feat_score, n_test_per_train = test_p_train),
          p_nb_paired = nb_corrected(base_score, feat_score, n_test_per_train = test_p_train),
          p_plain = t.test(base_score, feat_score, var.equal = TRUE)$p.value,
          tstat['bf']
        )
        newline = data.frame(measure = feat_info['full_title'],
                             method = m_typ,
                             as.list(newline))
        results_merged = plyr::rbind.fill(results_merged, newline)
      }
    }
  }
}


# score_dat = saveRDS(results_merged, "2021_ml_results_data_pre.rds")


t_boot(score_dat$feats_low_LR_scores,
       score_dat$baseline_low_LR_scores)

neatStats::t_neat(score_dat$feats_low_LR_scores,
                  score_dat$baseline_low_LR_scores,
                  #bf_added = T,
                  nonparametric = TRUE)
neatStats::t_neat(
  score_dat$feats_2_stud_LDA_scores,
  score_dat$baseline_stud_LDA_scores,
  #bf_added = T,
  nonparametric = TRUE,
  pair = T
)
