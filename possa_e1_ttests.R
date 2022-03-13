library('POSSA')

# Simple t-tests

# user-defined function to specify sample(s)
customSample_v1 = function(sample_size) {
  list(
    sample1 = rnorm(sample_size, mean = 0, sd = 10),
    sample2_h0 = rnorm(sample_size, mean = 0, sd = 10),
    sample2_h1 = rnorm(sample_size, mean = 5, sd = 10)
  )
}

# user-defined function to specify significance test(s)
customTest_v1 = function(sample1, sample2_h0, sample2_h1) {
  c(
    p_h0 = t.test(sample1, sample2_h0, 'less', var.equal = TRUE)$p.value,
    p_h1 = t.test(sample1, sample2_h1, 'less', var.equal = TRUE)$p.value
  )
}

# do.call(customTest_v1, customSample_v1(100))

df_ps_v1 = sim(fun_obs = customSample_v1,
               n_obs = c(27, 54, 81),
               fun_test = customTest_v1)

pow_results_1 = pow(df_ps_v1, alpha_locals = NA)
pow(df_ps_v1, alpha_locals = NA, design_fix = TRUE)

# cf. rpact results:
# summary(rpact::getSampleSizeMeans(rpact::getDesignGroupSequential(typeOfDesign = "P", informationRates = c(0.33333333, 0.66666667, 1), alpha = 0.05, beta = 0.1), alternative = 0.5))
# Number of subjects                   53.8  107.5  161.3
# meaning, for halves: c(53.8, 107.5,161.3)/2
# effect = 0.5, standard deviation = 1, power 90%.
# One-sided local significance level       0.0232 0.0232 0.0232

# gsDesign::gsDesign(k = 3, test.type = 1, n.fix = 81, beta = 0.1, alpha = 0.05, sfu = 'Pocock')

# pwr::pwr.t.test(d = 0.5, n = 81, alternative = 'greater')
# "power = 0.9361993"


df_ps_v1b = sim(fun_obs = customSample_v1,
               n_obs = c(41, 61, 82),
               fun_test = customTest_v1)
pow(df_ps_v1b, alpha_locals = NA, alpha_global = 0.025)

# summary(rpact::getSampleSizeMeans(rpact::getDesignGroupSequential(typeOfDesign = "P", informationRates = c(0.5, 0.75, 1), alpha = 0.025, beta = 0.15), alternative = 0.5))
# Number of subjects                         81.8  122.6  163.5
# c(81.8,  122.6, 163.5  ) / 2
# One-sided local significance level       0.0122 0.0122 0.0122

df_ps_v1c = sim(fun_obs = customSample_v1,
               n_obs = c(36, 72, 108),
               fun_test = customTest_v1)
pow(df_ps_v1c, alpha_locals = c(0.0013, 0.0013, NA), alpha_global = 0.01)
# summary(rpact::getSampleSizeMeans(rpact::getDesignGroupSequential(typeOfDesign = "HP", informationRates = c(0.33333333, 0.66666667, 1), alpha = 0.01, beta = 0.1), alternative = 0.5))
# Number of subjects                         71.9  143.8  215.8
# c(71.9, 143.8, 215.8) / 2
# One-sided local significance level       0.0013 0.0013 0.0087



# OF-correction

df_ps_v1d = sim(fun_obs = customSample_v1,
               n_obs = c(21, 42, 62, 83),
               fun_test = customTest_v1)
pow_results_1d_x = pow(df_ps_v1d, alpha_locals = c(0.0001, 0.0004, 0.0031, NA), alpha_global = 0.01)
pow_results_1d_y = pow(df_ps_v1d, alpha_locals = c(0.0001, 0.0004, 0.0031, 0.0089), alpha_global = 0.01)

# summary(rpact::getSampleSizeMeans(rpact::getDesignGroupSequential(typeOfDesign = "OF", informationRates = c(0.25, 0.5, 0.75, 1), alpha = 0.01), alternative = 0.5))
# Number of subjects                          41.5    82.9   124.4   165.8
# c(41.5, 82.9, 124.4, 165.8) / 2
# One-sided local significance level       <0.0001  0.0004  0.0031  0.0089


df_ps_v1e = sim(fun_obs = customSample_v1,
               n_obs = c(25, 51),
               fun_test = customTest_v1)
pow(df_ps_v1e, alpha_locals = c(0.0088, NA), alpha_global = 0.05)
pow(df_ps_v1e, alpha_locals = c(0.0088, 0.0467), alpha_global = 0.05)

# summary(rpact::getSampleSizeMeans(rpact::getDesignGroupSequential(typeOfDesign = "OF", informationRates = c(0.5, 1), alpha = 0.05), alternative = 0.5))
# Number of subjects                         50.9  101.9
# c(50.9,  101.9) / 2
# One-sided local significance level       0.0088 0.0467

df_ps_v1f = sim(fun_obs = customSample_v1,
               n_obs = c(10, 41, 51),
               fun_test = customTest_v1)
pow(df_ps_v1f, alpha_locals = c(0.0001, 0.0272, NA), alpha_global = 0.05)
pow(df_ps_v1f, alpha_locals = c(0.0001, NA, 0.0427), alpha_global = 0.05)
pow(df_ps_v1f, alpha_locals = c(NA, 0.0272, 0.0427), alpha_global = 0.05)
pow(df_ps_v1f, alpha_locals = c(0.0001, 0.0272, 0.0427), alpha_global = 0.05)

# summary(rpact::getSampleSizeMeans(rpact::getDesignGroupSequential(typeOfDesign = "OF", informationRates = c(0.2, 0.8, 1), alpha = 0.05), alternative = 0.5))
# Number of subjects                         20.6    82.3   102.8
# c(20.6,    82.3,   102.8 ) / 2
# One-sided local significance level       0.0001  0.0272  0.0427

#### Paired t-tests ####

# user-defined function to specify sample(s)
customSample_v2 = function(samp_size) {
  correlated_samples = faux::rnorm_multi(n = samp_size, vars = 3, mu = c(0, 0, 5), sd = 10, r = c(.3, .3, 0))
  list(
    v1 = correlated_samples$X1, # correlated with both X2 and X3
    v2_h0 = correlated_samples$X2, # correlated only with X1
    v2_h1 = correlated_samples$X3  # correlated only with X1
  )
}
customSample_v2_alt = function(v1, v2_h) {
  correlated_samples = faux::rnorm_multi(n = v1, vars = 3, mu = c(0, 0, 5), sd = 10, r = c(.3, .3, 0))
  list(
    v1 = correlated_samples$X1, # correlated with both X2 and X3
    v2_h0 = correlated_samples$X2, # correlated only with X1
    v2_h1 = correlated_samples$X3  # correlated only with X1
  )
}
# user-defined function to specify significance test(s)
customTest_v2 = function(v1, v2_h0, v2_h1) {
  c(
    p_h0 = t.test(v1, v2_h0, 'less', paired = TRUE, var.equal = TRUE)$p.value,
    p_h1 = t.test(v1, v2_h1, 'less', paired = TRUE, var.equal = TRUE)$p.value
  )
}

df_ps_v2 = sim(fun_obs = customSample_v2,
               n_obs = c(33, 66, 99),
               fun_test = customTest_v2)
df_ps_v2_alt = sim(
  fun_obs = customSample_v2_alt,
  n_obs = list(v1 = c(33, 66, 99), v2_h = c(33, 66, 99)),
  fun_test = customTest_v2
)
df_ps_v2b = sim(fun_obs = customSample_v2,
                n_obs = c(33, 44, 55, 66, 77, 88, 99),
                fun_test = customTest_v2)
df_ps_v2c = sim(fun_obs = customSample_v2,
                n_obs = c(11, 22, 99),
                fun_test = customTest_v2)
df_ps_v2d = sim(fun_obs = customSample_v2,
                n_obs = c(80, 90, 99),
                fun_test = customTest_v2)

pow(df_ps_v2, alpha_locals = NA) # 3 looks
pow(df_ps_v2_alt, alpha_locals = NA) # alternative syntax
pow(df_ps_v2b, alpha_locals = NA) # 7 looks
pow(df_ps_v2c, alpha_locals = NA) # early looks
pow(df_ps_v2d, alpha_locals = NA) # late looks
