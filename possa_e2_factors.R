# custom t-test function ####
ttest = function(x, y) {
  t_info = stats::t.test(x, y, paired = TRUE, var.equal = T)
  sdx = sd(x)
  sdy = sd(y)
  corr = cor(x, y)
  sd_p = sqrt((sdx ** 2 + sdy ** 2) - 2 * corr * sdx * sdy)
  return(list(
    pval = t_info$p.value,
    mean_diff = as.numeric(t_info$estimate),
    corr =  corr,
    smd = as.numeric(t_info$estimate) / sd_p
  ))
}

smd_paired = function(x, y) {
  return((mean(x) - mean(y)) / sd_p)
}

my_samp = function(samp_size, h1_mean, h1_corr) {
  correlated_samples = faux::rnorm_multi(
    n = samp_size,
    vars = 3,
    mu = c(0, 0, h1_mean),
    sd = 5,
    r = c(h1_corr, h1_corr, 0)
  )
  list(
    v1 = correlated_samples$X1, # correlated with both X2 and X3
    v2_h0 = correlated_samples$X2, # correlated only with X1
    v2_h1 = correlated_samples$X3  # correlated only with X1
  )
}

my_test = function(v1, v2_h0, v2_h1) {
  t0 = ttest(v2_h0, v1)
  t1 = ttest(v2_h1, v1)
  return(c(
    p_h0 = t0$pval,
    m_diff_0 = t0$mean_diff,
    corr_0 = t0$corr,
    smd_0 = t0$smd,
    p_h1 = t1$pval,
    m_diff_1 = t1$mean_diff,
    corr_1 = t1$corr,
    smd_1 = t1$smd
  ))
}

# do.call(my_test, my_samp(30, 1, .5))

# run simulation ####
# varied parameters
df_ps_facts = sim(
  fun_obs = list(
    my_samp,
    h1_mean = c(1.5, 2.5, 3.5),
    h1_corr = c(0, 0.5)
  ),
  n_obs = c(30, 60, 90),
  fun_test = my_test
)

# saveRDS(df_ps_facts, neatStats::path_neat("df_ps_saved.rds"))
# df_ps_facts = readRDS(neatStats::path_neat("df_ps_saved.rds"))

neatStats::peek_neat(df_ps_facts, c("m_diff_0", "m_diff_1"), group_by = c('h1_mean'))
neatStats::peek_neat(df_ps_facts, c("m_diff_1"), group_by = c('h1_mean', '.look'))
neatStats::peek_neat(df_ps_facts, c("corr_0", "corr_1"), group_by = c('h1_corr', '.look'))
# neatStats::peek_neat(df_ps_facts, c("smd_0"), group_by = c('h1_mean', 'h1_corr'))
neatStats::peek_neat(df_ps_facts, c("smd_1"), group_by = c('h1_mean', 'h1_corr'))

pow_results = pow(df_ps_facts, descr_cols = 'smd_1')

# pwr::pwr.t.test(d = 0.2150, n = 90, type = 'pair')
# power = 0.523029
# pwr::pwr.t.test(d = 0.5, n = 90, type = 'pair')
# power = 0.9968496

pow(df_ps_facts[df_ps_facts$h1_mean == 1.5 & df_ps_facts$h1_corr == 0,])

pow(df_ps_facts, alpha_global = .001)

pow(df_ps_facts, alpha_locals = NA)

xx = pow(df_ps_facts, alpha_locals = NA, fut_locals = c(0.5,0.5))
xx$df_3.5_0.5


