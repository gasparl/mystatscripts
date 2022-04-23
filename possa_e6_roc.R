# library('POSSA')
sample_AUC = function(n_group) {
  list(
    # polygraph-CIT
    ans_innocent = -runif(n = n_group, 0, 1),
    ans_guilty = sort(-rbeta(n = n_group, shape1 = 0.2, 1)),
    # RT-CIT
    rt_innocent = rnorm(n_group, mean = 0, sd = 30),
    rt_guilty_h0  = rnorm(n_group, mean = 41, sd = 30),
    rt_guilty_h1  = rnorm(n_group, mean = 60, sd = 30)
  )
}

test_AUC = function(ans_innocent,
                   ans_guilty,
                   rt_innocent,
                   rt_guilty_h0,
                   rt_guilty_h1) {
  predictors = data.frame(
    guilt = c(rep(0, length(ans_innocent)), rep(1, length(ans_guilty))),
    pred_ans = c(ans_innocent, ans_guilty),
    pred_rt_h0 = c(rt_innocent, rt_guilty_h0),
    pred_rt_h1 = c(rt_innocent, rt_guilty_h1)
  )

  # AUCs
  auc_ans = pROC::roc(
    response = predictors$guilt,
    predictor = predictors$pred_ans,
    levels = c(0, 1),
    direction =   "<" # second expected larger
  )
  auc_rt_h0 = pROC::roc(
    response = predictors$guilt,
    predictor = predictors$pred_rt_h0,
    levels = c(0, 1),
    direction =   "<"
  )
  auc_rt_h1 = pROC::roc(
    response = predictors$guilt,
    predictor = predictors$pred_rt_h1,
    levels = c(0, 1),
    direction =   "<"
  )


  return(
    c(
      p_h0 =
        pROC::roc.test(auc_ans,
                       auc_rt_h0,
                       pair = TRUE,
                       alternative = "less")$p.value,
      p_h1 =
        pROC::roc.test(auc_ans,
                       auc_rt_h1,
                       pair = TRUE,
                       alternative = "less")$p.value,
      auc_ans =
        as.numeric(pROC::auc(auc_ans)),
      auc_rt_h0 =
        as.numeric(pROC::auc(auc_rt_h0)),
      auc_rt_h1 =
        as.numeric(pROC::auc(auc_rt_h1))
    )
  )
}

# do.call(test_AUC, sample_AUC(100000))

df_ps_auc_fix = sim(
  fun_obs = sample_AUC,
  n_obs = 100,
  fun_test = test_AUC
)

pow_res_aov = pow(df_ps_auc_fix)

pow(df_ps_auc_fix, alpha_locals = NA, descr_cols = TRUE)

# sequential
df_ps_auc = sim(
  fun_obs = sample_AUC,
  n_obs = c(34, 69, 103),
  fun_test = test_AUC
)

# saveRDS(df_ps_auc, file = neatStats::path_neat('df_example_auc'))
# df_ps_auc = readRDS(neatStats::path_neat('df_example_auc'))

pow(df_ps_auc, alpha_locals = NA, descr_cols = TRUE)

pow(df_ps_auc, alpha_locals = c(0.0001, 0.0272, 0.0427), adjust = FALSE)
pow(df_ps_auc, alpha_locals = c(0.0001, 0.0272, NA))
pow(df_ps_auc, alpha_locals = c(0.0001, NA, 0.0427))
pow(df_ps_auc, alpha_locals = c(NA, 0.0272, 0.0427))

#summary(rpact::getSampleSizeMeans(rpact::getDesignGroupSequential(typeOfDesign = "OF", informationRates = c(0.333, 0.666, 1), alpha = 0.05), alternative = 0.5))
#
# Information rate                          33.3%  66.6%   100%
# Overall power                            0.0657 0.4871 0.8000
# Expected number of subjects                84.0
# Number of subjects                         34.3   68.6  103.0
# Cumulative alpha spent                   0.0015 0.0187 0.0500
# One-sided local significance level       0.0015 0.0181 0.0437
