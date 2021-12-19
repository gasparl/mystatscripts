library("neatStats")
mcnem = function(v1, v2) {
  propdat = stats::xtabs( ~ v2 + v1)
  return(
    stats::prop.test(
      propdat[2, 1],
      propdat[2, 1] + propdat[1, 2],
      alternative = 'greater',
      correct = FALSE
    )$p.value
  )
}

# simulation procedure to get p values
sim_pvals = function(n_iter = 5000,
                     cutoff_ans = 0.05,
                     cutoff_rt = 30,
                     fullsamp = 200,
                     corr = 0.5,
                     beta_shape = 0.225,
                     # lower for larger AUC
                     rt_mean1 = 36.8,
                     rt_mean2 = 52) {
  n_group = fullsamp / 2
  corr_mat <- matrix(corr, ncol = 2, nrow = 2)
  diag(corr_mat) <- 1

  cutoff_ans = -cutoff_ans
  list_vals = list()
  pb = txtProgressBar(
    min = 0,
    max = n_iter,
    initial = 0,
    style = 3
  )
  for (i in 1:n_iter) {
    setTxtProgressBar(pb, i)
    mvdat = MASS::mvrnorm(
      n = n_group,
      mu = c(0, 0),
      Sigma = corr_mat,
      empirical = TRUE
    )
    rx <- rank(mvdat[, 1], ties.method = "first")
    ry <- rank(mvdat[, 2], ties.method = "first")

    list_vals2 = list()
    for (rep in 1:2) {
      # ggpubr::ggdensity(-rbeta(n = 50000, 0.225, 1), xlab = 'p values')
      # ggpubr::gghistogram(rbeta(n = 50000, 0.225, 1), bins = 20, xlab = 'p values')
      # + ggplot2::theme(axis.text.y=ggplot2::element_blank())
      ans_g = sort(-rbeta(n = n_group, shape1 = beta_shape, 1))[rx]
      ans_i = -runif(n = n_group, 0, 1)
      rt_g0 = rnorm(n_group, mean = rt_mean1, sd = 33.6)
      rt_g1 = sort(rnorm(n_group, mean = rt_mean2, sd = 33.6))[ry]
      rt_i = rnorm(n_group, mean = 0, sd = 23.5)
      # corr_neat(ans_g, rt_g0)
      # corr_neat(ans_g, rt_g1)
      preds = data.frame(
        guilt = c(rep(0, length(ans_i)), rep(1, length(ans_g))),
        pred_ans = c(ans_i, ans_g),
        pred_rt0 = c(rt_i, rt_g0),
        pred_rt1 = c(rt_i, rt_g1)
      )

      # AUCs
      auc_ans = pROC::roc(
        response = preds$guilt,
        predictor = preds$pred_ans,
        levels = c(0, 1),
        direction =   "<" # second expected larger
      )

      auc_rt0 = pROC::roc(
        response = preds$guilt,
        predictor = preds$pred_rt0,
        levels = c(0, 1),
        direction =   "<"
      )
      auc_rt1 = pROC::roc(
        response = preds$guilt,
        predictor = preds$pred_rt1,
        levels = c(0, 1),
        direction =   "<"
      )

      # props tests

      # make classifications using cutoffs
      preds$cut_preset_ans = (preds$pred_ans > cutoff_ans) == preds$guilt
      preds$cut_preset_rt0 = (preds$pred_rt0 > cutoff_rt) == preds$guilt
      preds$cut_preset_rt1 = (preds$pred_rt1 > cutoff_rt) == preds$guilt
      preds$cut_best_ans = (preds$pred_ans > as.numeric(pROC::coords(auc_ans, x = "best")$threshold)[1]) == preds$guilt
      preds$cut_best_rt0 = (preds$pred_rt0 > as.numeric(pROC::coords(auc_rt0, x = "best")$threshold)[1]) == preds$guilt
      preds$cut_best_rt1 = (preds$pred_rt1 > as.numeric(pROC::coords(auc_rt1, x = "best")$threshold)[1]) == preds$guilt

      list_vals2[[length(list_vals2) + 1]] =
        c(
          p_vals_auc0 =
            pROC::roc.test(
              auc_ans,
              auc_rt0,
              pair = TRUE,
              alternative = "less"
            )$p.value
          ,
          p_vals_auc1 =
            pROC::roc.test(
              auc_ans,
              auc_rt1,
              pair = TRUE,
              alternative = "less"
            )$p.value
          ,
          auc_ans =
            as.numeric(pROC::auc(auc_ans))
          ,
          auc_rt0 =
            as.numeric(pROC::auc(auc_rt0))
          ,
          auc_rt1 =
            as.numeric(pROC::auc(auc_rt1))
          ,
          p_vals_prop_preset_0 =
            mcnem(preds$cut_preset_ans, preds$cut_preset_rt0)
          ,
          p_vals_prop_preset_1 =
            mcnem(preds$cut_preset_ans, preds$cut_preset_rt1)
          ,
          p_vals_prop_best_0 =
            mcnem(preds$cut_best_ans, preds$cut_best_rt0)
          ,
          p_vals_prop_best_1 =
            mcnem(preds$cut_best_ans, preds$cut_best_rt1)
          ,
          prop_preset_ans = mean(preds$cut_preset_ans),
          prop_preset_rt0 = mean(preds$cut_preset_rt0),
          prop_preset_rt1 = mean(preds$cut_preset_rt1),
          prop_best_ans = mean(preds$cut_best_ans),
          prop_best_rt0 = mean(preds$cut_best_rt0),
          prop_best_rt1 = mean(preds$cut_best_rt1)
        )
    }
    df_3set = as.data.frame(do.call(rbind, list_vals2))

    list_vals[[length(list_vals) + 1]] =
      c(
        iter = i,
        auc_ans = mean(df_3set$auc_ans),
        auc_rt0 = mean(df_3set$auc_rt0),
        auc_rt1 = mean(df_3set$auc_rt1),

        p_vals_auc0_bh = min(p.adjust(df_3set$p_vals_auc0, method = 'BH')),
        p_vals_auc1_bh = min(p.adjust(df_3set$p_vals_auc1, method = 'BH')),
        p_vals_auc0_hmp = as.numeric(harmonicmeanp::p.hmp(df_3set$p_vals_auc0, L =
                                                            3)),
        p_vals_auc1_hmp = as.numeric(harmonicmeanp::p.hmp(df_3set$p_vals_auc1, L =
                                                            3)),

        prop_preset_ans = mean(df_3set$prop_preset_ans),
        prop_preset_rt0 = mean(df_3set$prop_preset_rt0),
        prop_preset_rt1 = mean(df_3set$prop_preset_rt1),
        prop_best_ans = mean(df_3set$prop_best_ans),
        prop_best_rt0 = mean(df_3set$prop_best_rt0),
        prop_best_rt1 = mean(df_3set$prop_best_rt1),

        p_vals_prop_preset_0_mean = mean(df_3set$p_vals_prop_preset_0),
        p_vals_prop_preset_1_mean = mean(df_3set$p_vals_prop_preset_1),
        p_vals_prop_preset_0_bh = min(p.adjust(
          df_3set$p_vals_prop_preset_0,
          method = 'BH'
        )),
        p_vals_prop_preset_1_bh = min(p.adjust(
          df_3set$p_vals_prop_preset_1,
          method = 'BH'
        )),
        p_vals_prop_preset_0_hmp = as.numeric(harmonicmeanp::p.hmp(df_3set$p_vals_prop_preset_0, L =
                                                                     3)),
        p_vals_prop_preset_1_hmp = as.numeric(harmonicmeanp::p.hmp(df_3set$p_vals_prop_preset_1, L =
                                                                     3)),

        p_vals_prop_best_0_bh = min(p.adjust(df_3set$p_vals_prop_best_0,
                                             method = 'BH')),
        p_vals_prop_best_1_bh = min(p.adjust(df_3set$p_vals_prop_best_1,
                                             method = 'BH')),
        p_vals_prop_best_0_hmp = as.numeric(harmonicmeanp::p.hmp(df_3set$p_vals_prop_best_0, L =
                                                                   3)),
        p_vals_prop_best_1_hmp = as.numeric(harmonicmeanp::p.hmp(df_3set$p_vals_prop_best_1, L =
                                                                   3))

      )
  }
  close(pb)
  df_pvals = as.data.frame(do.call(rbind, list_vals))
  get_pow(
    df_pvals,
    cols = c(
      'p_vals_prop_best_0_bh',
      'p_vals_prop_best_0_hmp',
      'p_vals_prop_preset_0_bh',
      'p_vals_prop_preset_0_hmp'
    )
  )
  cat('MAIN:', fill = TRUE)
  get_pow(df_pvals,
          cols = c('p_vals_auc0_bh',
                   'p_vals_auc0_hmp'))
  for (ronum in c(2, 5)) {
    cat(
      'AUCs: AR:',
      ro(mean(df_pvals$auc_ans), ronum),
      'RT H0:',
      ro(mean(df_pvals$auc_rt0), ronum),
      'RT H1:',
      ro(mean(df_pvals$auc_rt1), ronum),
      'Gain:',
      ro(mean(df_pvals$auc_rt1) - mean(df_pvals$auc_ans), ronum),
      fill = TRUE
    )
  }
  return(df_pvals)
}

get_pow = function(dat, cols, alpha = 0.05) {
  for (colname in cols) {
    p0 = dat[[colname]]
    p1 = dat[[sub('0', '1', colname)]]
    cat(
      '                       -- ',
      sub('0', 'x', colname),
      ' --\nType I error: ',
      mean(p0 < alpha),
      '\nPower: ',
      mean(p1 < alpha),
      '\n',
      sep = ''
    )
  }
}

# run simulations

set.seed(0)
df_ps = sim_pvals()
set.seed(1)
df_ps = sim_pvals(corr = 0.1)
set.seed(2)
df_ps = sim_pvals(rt_mean2 = 48)


# larger base AUC
set.seed(3)
df_ps = sim_pvals(beta_shape = 0.16,
                  # lower value for larger AR-CIT AUC
                  rt_mean1 = 45,
                  rt_mean2 = 65)
# lower base AUC
set.seed(4)
df_ps = sim_pvals(beta_shape = 0.33,
                  rt_mean1 = 28,
                  rt_mean2 = 40)


# Check descriptives
neatStats::peek_neat(df_ps, values = c('auc_ans', 'auc_rt0', 'auc_rt1'))
neatStats::peek_neat(df_ps,
                     values = c('prop_best_ans', 'prop_best_rt0', 'prop_best_rt1'))
neatStats::peek_neat(df_ps,
                     values = c('prop_preset_ans', 'prop_preset_rt0', 'prop_preset_rt1'))

# get_pow(df_ps,
#         cols = c('p_vals_auc0_hmp'),
#         alpha = .05)
