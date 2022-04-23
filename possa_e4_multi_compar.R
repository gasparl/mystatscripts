# library('POSSA')

multiSample = function(samp_size) {
  corr_vars_h0 = faux::rnorm_multi(
    n = samp_size,
    vars = 3,
    mu = 0,
    sd = 10,
    #r = 0
    r = c(0, 0.4, 0.8)
  )
  corr_vars_h1 = faux::rnorm_multi(
    n = samp_size,
    vars = 3,
    mu = 5,
    sd = 10,
    #r = 0
    r = c(0, 0.4, 0.8)
  )
  v1 = rnorm(samp_size, mean = 0, sd = 10)
  list(
    grp_1_test_base = v1,
    grp_2_test1_h0 = corr_vars_h0$X1,
    grp_2_test1_h1 = corr_vars_h1$X1,
    grp_2_test2_h0 = corr_vars_h0$X2,
    grp_2_test2_h1 = corr_vars_h1$X2,
    grp_2_test3_h0 = corr_vars_h0$X3,
    grp_2_test3_h1 = corr_vars_h1$X3
  )
}

multiTest = function(grp_1_test_base,
                     grp_2_test1_h0,
                     grp_2_test1_h1,
                     grp_2_test2_h0,
                     grp_2_test2_h1,
                     grp_2_test3_h0,
                     grp_2_test3_h1) {
  c(
    p_test1_h0 = t.test(grp_1_test_base, grp_2_test1_h0, 'less', var.equal = TRUE)$p.value,
    p_test1_h1 = t.test(grp_1_test_base, grp_2_test1_h1, 'less', var.equal = TRUE)$p.value,
    p_test2_h0 = t.test(grp_1_test_base, grp_2_test2_h0, 'less', var.equal = TRUE)$p.value,
    p_test2_h1 = t.test(grp_1_test_base, grp_2_test2_h1, 'less', var.equal = TRUE)$p.value,
    p_test3_h0 = t.test(grp_1_test_base, grp_2_test3_h0, 'less', var.equal = TRUE)$p.value,
    p_test3_h1 = t.test(grp_1_test_base, grp_2_test3_h1, 'less', var.equal = TRUE)$p.value
  )
}

# do.call(multiTest, multiSample(100))

df_ps_multi = sim(
  fun_obs = multiSample,
  n_obs = c(27, 54, 81),
  fun_test = multiTest,
  n_iter = 100
)

# df_ps_multi2 = df_ps_multi
# correlated
pow(df_ps_multi2, alpha_locals = NA)
pow(df_ps_multi2,
    alpha_locals = NA,
    multi_logic_a = 'any')

# noncorrelated
pow(df_ps_multi, alpha_locals = NA)
pow(df_ps_multi, alpha_locals = NA, multi_logic_a = 'all')
pow(df_ps_multi, alpha_locals = NA, multi_logic_a = all)
pow(df_ps_multi, alpha_locals = NA, multi_logic_a = 'any')

# different group sizes
multiSampleX = function(grp_1, grp_2) {
  corr_vars_h0 = faux::rnorm_multi(
    n = grp_2,
    vars = 3,
    mu = 0,
    sd = 10,
    #r = 0
    r = c(0, 0.4, 0.8)
  )
  corr_vars_h1 = faux::rnorm_multi(
    n = grp_2,
    vars = 3,
    mu = 4,
    sd = 10,
    #r = 0
    r = c(0, 0.4, 0.8)
  )
  v1 = rnorm(grp_1, mean = 0, sd = 10)
  list(
    grp_1_test_base = v1,
    grp_2_test1_h0 = corr_vars_h0$X1, # correlates with X3 (.4)
    grp_2_test1_h1 = corr_vars_h1$X1,
    grp_2_test2_h0 = corr_vars_h0$X2, # correlates with X3 (.8)
    grp_2_test2_h1 = corr_vars_h1$X2,
    grp_2_test3_h0 = corr_vars_h0$X3, # correlates with X1 and X3
    grp_2_test3_h1 = corr_vars_h1$X3
  )
}

df_ps_multi_x = sim(
  fun_obs = multiSampleX,
  n_obs = list(grp_1 = c(27, 54, 81),
               grp_2 = c(60, 90, 120)),
  fun_test = multiTest
)

# alpha and futility bounds for multiple as well as per p value, and per different multi_logic_fut

# note: grp_3 correlates with grp_1 (r = .4) and grp_2 (r = .8)
# grp_1 and grp_2 do not correlate (r = 0)

pow(df_ps_multi_x,
    alpha_locals = c(0.7, 0.5, 0.3))
pow(df_ps_multi_x,
    alpha_locals = list(
      p_test1 = c(0.7, 0.5, 0.3),
      p_test2 = c(0.7, 0.5, 0.3),
      p_test3 = c(0.7, 0.5, 0.3)
    ))
pow(df_ps_multi_x,
    alpha_locals = list(
      p_test1 = c(0.7, 0.5, 0.3),
      p_test2 = c(0.7, 0.5, 0.3)
    ))
pow(df_ps_multi_x,
    alpha_locals = list(
      p_test2 = c(0.7, 0.5, 0.3),
      p_test3 = c(0.7, 0.5, 0.3)
    ))

# saveRDS(df_ps_multi_x, file = neatStats::path_neat('df_example_x'))
# df_ps_multi_x = readRDS(neatStats::path_neat('df_example_x'))
pow(
  df_ps_multi_x,
  alpha_locals = list(p_test2 = c(0.7, 0.5, 0.3))
)
pow(
  df_ps_multi_x,
  alpha_locals = list(p_test2 = c(0.7, 0.5, 0.3)),
  alpha_loc_nonstop = list(p_test1 = c(0.7, 0.5, 0.3),
                           p_test3 = c(0.7, 0.5, 0.3))
)
pow(
  df_ps_multi_x,
  alpha_locals = list(p_test2 = c(0.7, 0.5, 0.3)),
  alpha_loc_nonstop = list(p_test1 = c(0.03, 0.02, 0.01),
                           p_test3 = c(0.03, 0.02, 0.01))
)
pow(
  df_ps_multi_x,
  alpha_locals = list(p_test2 = c(0.7, 0.5, 0.3)),
  alpha_loc_nonstop = list(p_test1 = c(0.05, 0.05, 0.05),
                           p_test3 = c(0.05, 0.05, 0.05))
)

# futilities
pow(df_ps_multi_x,
    fut_locals =  c(0.6, 0.4))
pow(df_ps_multi_x,
    fut_locals = list(
      p_test1 = c(0.6, 0.4),
      p_test2 = c(0.6, 0.4),
      p_test3 = c(0.6, 0.4)
    ))

pow(df_ps_multi_x,
    fut_locals = list(p_test2 = c(0.6, 0.4)))

pow(
  df_ps_multi_x,
  fut_locals = list(p_test1 = c(0.5, 0.3),
                    p_test2 = c(0.5, 0.3)),
  multi_logic_fut = 'any' # 'all'/'any'
  #, multi_logic_global = 'all'
)

# alphas & futilities
pow(
  df_ps_multi_x,
  alpha_locals =  list(p_test2 = c(0, 0, 0.5), p_test3 = c(0, 0, 0.5)),
  fut_locals = list(p_test2 = c(0.5, 0.3),
                    p_test3 = c(0.5, 0.3))  #, multi_logic_fut = 'any'
  #, multi_logic_global = 'all'
)
# note: grp_3 correlates with grp_1 (r = .4) and grp_2 (r = .8)
# grp_1 and grp_2 do not correlate (r = 0)
pow(
  df_ps_multi_x,
  alpha_locals =  list(p_test2 = c(0, 0, 0.5), p_test1 = c(0, 0, 0.5)),
  fut_locals = list(p_test2 = c(0.5, 0.3),
                    p_test1 = c(0.5, 0.3))  #, multi_logic_fut = 'any'
  #, multi_logic_global = 'all'
)

pow(
  df_ps_multi_x,
  alpha_locals = NA,
  fut_locals = list(p_test1 = c(0.5, 0.3),
                    p_test2 = c(0.5, 0.3)),
  multi_logic_fut = 'any'
)
pow(
  df_ps_multi_x,
  alpha_locals = NA,
  fut_locals = list(p_test1 = c(0.5, 0.3),
                    p_test2 = c(0.5, 0.3)),
  multi_logic_a = 'any',
  multi_logic_fut = 'all'
)
