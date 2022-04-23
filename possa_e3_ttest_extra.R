library('POSSA')

# unequal sample sizes and variances

# Welch's t-test with added info
wtest = function(x, y) {
  t_info = stats::t.test(x, y, 'less')
  sdx = sd(x)
  sdy = sd(y)
  n1 = length(x)
  n2 = length(y)
  sd_p = sqrt(((n1 - 1) * (sdx ** 2) + (n2 - 1) * (sdy ** 2)) / (n1 + n2 - 2))
  return(list(
    pval = t_info$p.value,
    mean_diff = as.numeric(t_info$estimate),
    sd1 =  sdx,
    sd2 =  sdy,
    smd = as.numeric(t_info$estimate) / sd_p
  ))
}

# effect size equality comparison
# neatStats::t_neat(bayestestR::distribution_normal(5000, 0, 10),bayestestR::distribution_normal(5000, 5, 10))$stats
# neatStats::t_neat(bayestestR::distribution_normal(5000, 0, 13),bayestestR::distribution_normal(5000, 5, 4))$stats

samp_uneq = function(sample1, sample2_h) {
  list(
    sample1 = rnorm(sample1, mean = 0, sd = 4),
    sample2_h0 = rnorm(sample2_h, mean = 0, sd = 13),
    sample2_h1 = rnorm(sample2_h, mean = 5, sd = 13)
  )
}

test_w = function(sample1, sample2_h0, sample2_h1) {
  t0 = wtest(sample1, sample2_h0)
  t1 = wtest(sample1, sample2_h1)
  return(c(
    p_h0 = t0$pval,
    m_diff_0 = t0$mean_diff,
    sd1_0 = t0$sd1,
    sd2_0 = t0$sd2,
    smd_0 = t0$smd,
    p_h1 = t1$pval,
    m_diff_1 = t1$mean_diff,
    sd1_1 = t1$sd1,
    sd2_1 = t1$sd2,
    smd_1 = t1$smd
  ))
}

# do.call(test_w, samp_uneq(30, 60))

# run simulation ####
# varied parameters
df_ps_w1 = sim(
  fun_obs = samp_uneq,
  n_obs = c(27, 54, 81),
  fun_test = test_w
)
w_res1 = pow(df_ps_w1, alpha_locals = NA, descr_cols = TRUE)

df_ps_w2 = sim(
  fun_obs = samp_uneq,
  n_obs = list(
    sample1 = c(17, 44, 71),
    sample2_h = c(37, 64, 91)
  ),
  fun_test = test_w
)
w_res2 = pow(df_ps_w2, alpha_locals = NA, descr_cols = TRUE)


# ranked data ####

ordinalSample = function(s, m) {
  findInterval(rnorm(s, mean = m, sd = 2), vec = c(-Inf, 1, 2, 3, 4, Inf))
  findInterval(
    5*rbeta(n = s, shape1 = .3, 1)+m-1, vec = c(-Inf, 1, 2, 3, 4, Inf))
}

# neatStats::peek_neat(data.frame(x = ordinalSample(100, 2)), 'x', f_plot = neatStats::plot_neat)
# neatStats::peek_neat(data.frame(x = ordinalSample(100, 2)), 'x', f_plot = neatStats::plot_neat)

# user-defined function to specify sample(s)
customSample_Rank = function(sample_size) {
  list(
    sample1 = ordinalSample(sample_size, 2),
    sample2_h0 = ordinalSample(sample_size, 2),
    sample2_h1 = ordinalSample(sample_size, 3)
  )
}

# user-defined function to specify significance test(s)
customTest_Wilc = function(sample1, sample2_h0, sample2_h1) {
  c(
    p_h0 = wilcox.test(sample1, sample2_h0, alternative = 'less')$p.value,
    p_h1 = wilcox.test(sample1, sample2_h1, alternative = 'less')$p.value
  )
}

# do.call(customTest_Wilc, customSample_Rank(100))

df_ps_Rank_fix = sim(fun_obs = customSample_Rank,
                 n_obs = 50,
                 fun_test = customTest_Wilc)

pow(df_ps_Rank_fix)

df_ps_Rank_fix3 = sim(fun_obs = customSample_Rank,
                      n_obs = c(50, 60, 70),
                      fun_test = customTest_Wilc)

pow_results_Rank3 = pow(df_ps_Rank_fix3, design_fix = TRUE)
pow(df_ps_Rank_fix3, alpha_locals = NA, design_fix = FALSE)



# f1 = function(n) {ordinalSample(n, 2)}
# f2 = function(n) {ordinalSample(n, 3)}
# MKpower::sim.ssize.wilcox.test(f1, f2, power = 0.9, n.min = 50, n.max = 70, alternative = 'less')
# n = 50, 60, 70
# emp.power = 0.7510, 0.8073, 0.8674
# pow(df_ps_Rank_fix3, design_fix = TRUE, alpha_global = .025)
# MKpower::sim.ssize.wilcox.test(f1, f2, power = 0.9, n.min = 50, n.max = 70, alternative = 'less', sig.level = .025)

# same as in plain t-test
df_ps_Rank = sim(fun_obs = customSample_Rank,
                 n_obs = c(27, 54, 81),
                 fun_test = customTest_Wilc)
pow(df_ps_Rank, alpha_locals = NA)




# ranked data: Wilcox vs ttests

customTest_ttest = function(sample1, sample2_h0, sample2_h1) {
  c(
    p_h0 = t.test(sample1, sample2_h0, alternative = 'less')$p.value,
    p_h1 = t.test(sample1, sample2_h1, alternative = 'less')$p.value
  )
}

df_ps_Rank_wilc = sim(fun_obs = customSample_Rank,
                      n_obs = c(20),
                      fun_test = customTest_Wilc)
df_ps_Rank_ttest = sim(fun_obs = customSample_Rank,
                       n_obs = c(20),
                       fun_test = customTest_ttest)

pow(df_ps_Rank_wilc)
pow(df_ps_Rank_ttest)

df_ps_Rank_wilc3 = sim(fun_obs = customSample_Rank,
                       n_obs = c(20, 40),
                       fun_test = customTest_Wilc)
df_ps_Rank_ttest3 = sim(fun_obs = customSample_Rank,
                        n_obs = c(20, 40),
                        fun_test = customTest_ttest)
pow(df_ps_Rank_wilc3, alpha_locals = NA)
pow(df_ps_Rank_ttest3, alpha_locals = NA)
