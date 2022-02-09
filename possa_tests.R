setwd(neatStats::path_neat())
source("possa_sketch.R")

t_test = function(x, y) {
  t_info = stats::t.test(x, y, var.equal = T)
  n1 = length(x)
  n2 = length(y)
  nom = (n1 - 1) * (sd(x) ** 2) + (n2 - 1) * (sd(y) ** 2)
  sd_p = sqrt(nom / (n1 + n2 - 2))
  t_info$mean_diff = mean(x) - mean(y)
  t_info$smd = t_info$mean_diff / sd_p
  return(t_info)
}

# user-defined function to specify sample(s)

# from rpact:
design <- rpact::getDesignGroupSequential(typeOfDesign = "P", informationRates = c(0.33333333,
                                                                            0.66666667, 1), alpha = 0.05, beta = 0.1, sided = 2, tolerance = 1e-08)
summary(rpact::getSampleSizeMeans(design, meanRatio = FALSE, thetaH0 = 0, normalApproximation = FALSE,
                           alternative = 0.5, stDev = 1, groups = 2, allocationRatioPlanned = 1))
# Number of subjects                   65.2  130.5  195.7
# effect = 0.5, standard deviation = 1, power 90%.
# Two-sided local significance level 0.0221 0.0221 0.0221

# now same by simulation

custom_sample_v1 = function(v1, v2_h) {
  samples = list()
  samples$v1 = rnorm(v1, mean = 0, sd = 1)
  samples$v2_h0 = rnorm(v2_h, mean = 0, sd = 1)
  samples$v2_h1 = rnorm(v2_h, mean = 0.5, sd = 1)
  return(samples)
}


# user-defined function to specify significance test(s)
custom_test_v1 = function(sampl) {
  t0 = t_test(sampl$v2_h0, sampl$v1)
  t1 = t_test(sampl$v2_h1, sampl$v1)
  return(
    c(
      p_h0 = t0$p.value,
      p_h1 = t1$p.value,
      cohens_d_0 = t0$smd,
      m_diff_0 = mean(sampl$v2_h0) - mean(sampl$v1),
      cohens_d_1 = t0$smd,
      m_diff_1 = mean(sampl$v2_h1) - mean(sampl$v1)
    )
  )
}

df_ps = sim_pvals(
  f_sample = custom_test_v1,
  n_obs = c(65, 131, 196),
  f_test = custom_test_v1,
  n_iter = 5000
)



custom_sample2 = function(v1, v2_h, h1_mean, h1_sd) {
  samples = list()
  samples$v1 = rnorm(v1, mean = 0, sd = 1)
  samples$v2_h0 = rnorm(v2_h, mean = 0, sd = 1)
  samples$v2_h1 = rnorm(v2_h, mean = h1_mean, sd = h1_sd)
  return(samples)
}


setwd(neatStats::path_neat())
# saveRDS(df_ps, "df_ps_example.rds")
# df_ps = readRDS("df_ps_example.rds")

# saveRDS(df_stops, "dat_temp.rds")
# df_stps = readRDS("dat_temp.rds")

# run simulation ####
# varied parameters
df_ps = sim_pvals(
  f_sample = list(
    custom_sample2,
    h1_mean = c(0.5, 1, 1.5),
    h1_sd = c(1, 1.5)
  ),
  n_obs = c(30, 60, 90),
  f_test = custom_test,
  n_iter = 100
)

# unvaried 1
df_ps = sim_pvals(
  f_sample = custom_sample1,
  n_obs = c(30, 60, 90),
  f_test = custom_test,
  n_iter = 2000
)

# neatStats::peek_neat(df_ps, c("cohens_d_0", "cohens_d_1"), group_by = 'look')
# neatStats::peek_neat(df_ps, c("m_diff_0", "m_diff_1"), group_by = 'look')

neatStats::peek_neat(df_ps, c("m_diff_0", "m_diff_1"), group_by = c('h1_mean'))

# unvaried 2
df_ps = sim_pvals(
  f_sample = custom_sample,
  n_obs = list(v1 = c(30, 60, 90),
               v2_h = c(30, 60, 90)),
  f_test = custom_test,
  n_iter = 1000
)

# get power for conventional alpha
pow_results = get_pow(df_ps)

# again for small alpha
get_pow(df_ps, alpha_global = .001)

# at the moment it's probably senselessly precise
get_pow(df_ps, alpha_global = .1735)



# to check effect size
tx = t_test(
  x = bayestestR::distribution_normal(1000, 0.5, sd = 1),
  y = bayestestR::distribution_normal(1000, 0, sd = 1)
)

