# library("neatStats")
mcnem = function(v1, v2) {
  propdat = stats::xtabs(~ v2 + v1)
  return(stats::prop.test(propdat[2, 1],
                          propdat[2, 1] + propdat[1, 2],
                          correct = FALSE)$p.value)
}

# simulation procedure to get p values
sim_pvals = function(cutoff_ans = 0.05, cutoff_rt = 30, n_iter = 1000, fullsamp = 200, corr = 0.5) {
  groupsamp = fullsamp / 2
  corr_mat <- matrix(corr, ncol = 2, nrow = 2)
  diag(corr_mat) <- 1

  cutoff_ans = -cutoff_ans
  ps_guilt =
    -replicate(n_iter, t.test(rnorm(100, mean = 1, sd = 3))$p.val, simplify = T)
  ps_inno =
    -replicate(n_iter, t.test(rnorm(100, mean = 0, sd = 1))$p.val, simplify = T)
  list_vals = list()
  for (i in 1:n_iter) {
    mvdat = MASS::mvrnorm(n = groupsamp, mu = c(0, 0), Sigma = corr_mat, empirical = TRUE)
    rx <- rank(mvdat[ , 1], ties.method = "first")
    ry <- rank(mvdat[ , 2], ties.method = "first")

    list_vals2 = list()
    for (rep in 1:3) {
      ans_g = sort(sample(ps_guilt, groupsamp))[rx]
      ans_i = sample(ps_inno, groupsamp)
      rt_g0 = sort(rnorm(groupsamp, mean = 36.8, sd = 33.6))[ry]
      rt_g1 = sort(rnorm(groupsamp, mean = 60, sd = 33.6))[ry]
      rt_i = rnorm(groupsamp, mean = 0, sd = 23.5)
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
            pROC::roc.test(auc_ans, auc_rt0, pair = TRUE, alternative = "less")$p.value
          ,
          p_vals_auc1 =
            pROC::roc.test(auc_ans, auc_rt1, pair = TRUE, alternative = "less")$p.value
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

    # TODO
    # calc integrated p vals with averaging, with bonferroni and BH corrected minimum p value, or as HMP


    list_vals[[length(list_vals) + 1]] =
      c(
        iter = i,
        p_vals_auc0_mean = mean(df_3set$p_vals_auc0),
        p_vals_auc1_mean = mean(df_3set$p_vals_auc1),
        auc_ans = mean(df_3set$auc_ans),
        auc_rt0 =mean(df_3set$auc_rt0),
        auc_rt1 =meanc(df_3set$auc_rt1),
        p_vals_prop_preset_0_mean = mean(df_3set$p_vals_prop_preset_0),
        p_vals_prop_preset_1_mean = mean(df_3set$p_vals_prop_preset_1),
        p_vals_prop_best_0_mean = mean(df_3set$p_vals_prop_best_0),
        p_vals_prop_best_1_mean = mean(df_3set$p_vals_prop_best_1),
        prop_preset_ans = mean(df_3set$prop_preset_ans),
        prop_preset_rt0 = mean(df_3set$prop_preset_rt0),
        prop_preset_rt1 = mean(df_3set$prop_preset_rt1),
        prop_best_ans = mean(df_3set$prop_best_ans),
        prop_best_rt0 = mean(df_3set$prop_best_rt0),
        prop_best_rt1 = mean(df_3set$prop_best_rt1)
      )
  }
  df_pvals = as.data.frame(do.call(rbind, list_vals))
  return(df_pvals[order(df_pvals$iter, df_pvals$look), ])
}

get_pow = function(p_values, alpha = 0.05) {
  msamp = max(p_values$n)
  cat(
    '-- FIXED DESIGN\nN(total) = ',
    msamp * 2,
    '\nType I error: ',
    mean(p_values$p_h0 < alpha),
    '\nPower: ',
    mean(p_values$p_h1[p_values$n == msamp] < alpha),
    '\n',
    sep = ''
  )
}

# ggpubr::gghistogram(ps_guilt, binwidth = 0.05)
# ggpubr::gghistogram(ps_inno, binwidth = 0.05)
# ans_auc = t_neat(ps_inno, ps_guilt, auc_added = T, plots = T, norm_tests = FALSE)
# roc_neat(list(ans_auc$roc_obj))


# set.seed(2021) # you could do set.seed to get exactly what I got
# but probably doesn't matter here

# run simulation
df_ps = sim_pvals(custom_sample, custom_test, list(n = c(200)), 10000)

# get power for conventional alpha
get_pow(df_ps, alpha = .05)

# again for small alpha
get_pow(df_ps, alpha = .001)

# at the moment it's probably senselessly precise
get_pow(df_ps, alpha = .1735)

