# A first sketch for Power Simulation for Sequential Analysis

# user-defined function to specify sample(s)
custom_sample =  function(n) {
  samples = list()
  samples$v1a_n = rnorm(n, mean = 0, sd = 1)
  samples$v1b_n = rnorm(n, mean = 0, sd = 1)
  samples$v2_n = rnorm(n, mean = 0.5, sd = 1)
  return(samples)
}

# user-defined function to specify significance test(s)
custom_test = function(sampl) {
  p_val0 = t.test(sampl$v1a_n, sampl$v1b_n)$p.value
  p_val1 = t.test(sampl$v1a_n, sampl$v2_n)$p.value
  return(c(p_h0 = p_val0, p_h1 = p_val1))
}

# simulation procedure to get p values
simmed_pvals = function(f_sample, f_test, n_obs, n_iter = 1000) {
  n_look = length(n_obs[[1]])
  if (!all(sapply(n_obs, length) == n_look)) {
    stop('The lengths of the "n_obs" values are unequal.')
  }
  obs_per_it = list()
  for (lk in 1:n_look) {
    obs_per_it[[lk]] = sapply(n_obs, '[', lk)
  }
  list_vals = list()
  for (i in 1:n_iter) {
    samples = f_sample(obs_per_it[[length(obs_per_it)]]['n'])
    for (lk in 1:n_look) {
      # here sample should be subsampled based on variable name
      # e.g. v1a_n with "n" because of the "_n"
      # for this sketch it's all simply "n"
      test_samp = lapply(samples, function(x) {
        sample(x, obs_per_it[[lk]]['n'])
      })
      list_vals[[length(list_vals) + 1]] = c(obs_per_it[[lk]], f_test(test_samp))
    }
  }
  df_pvals = as.data.frame(do.call(rbind, list_vals))
  # class(df_pvals) = c(class(df_pvals), "pvals")
  return(df_pvals)
}

df_ps = simmed_pvals(custom_sample, custom_test, list(n = c(50, 100, 150)), 1000)


# power calculation
# would be eventually generalizable for any outcome
# for now only works with the variable names above
get_pow = function(p_values, alpha = 0.05) {
  cat('--- FIXED DESIGN ---', fill = T)
  msamp = max(p_values$n)
  cat('Type I error:', mean(p_values$p_h0 < alpha), fill = T)
  cat(
    'Power:',
    mean(p_values$p_h1[p_values$n == msamp] < alpha),
    '( cf. via pwr:',
    pwr::pwr.t.test(n = msamp, sig.level = alpha, d = .5)$power,
    ')'
  )
}
get_pow(df_ps, alpha = .01)
