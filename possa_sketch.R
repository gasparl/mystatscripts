# A first sketch for Power Simulation for Sequential Analysis

# simulation procedure to get p values
sim_pvals = function(f_sample, f_test, n_obs, n_iter = 1000) {
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
    samples = f_sample(obs_per_it[[n_look]]['n'])
    list_vals[[length(list_vals) + 1]] =
      c(iter = i,
        look = n_look,
        obs_per_it[[n_look]],
        f_test(samples))
    for (lk in (n_look - 1):1) {
      # here sample should be subsampled based on variable name
      # e.g. v1a_n with "n" because of the "_n"
      # for this sketch it's all simply with "n"
      samples = lapply(samples, function(x) {
        sample(x, obs_per_it[[lk]]['n'])
      })
      list_vals[[length(list_vals) + 1]] =
        c(iter = i,
          look = lk,
          obs_per_it[[lk]],
          f_test(samples))
    }
  }
  df_pvals = as.data.frame(do.call(rbind, list_vals))
  # class(df_pvals) = c(class(df_pvals), "pvals")
  return(df_pvals[order(df_pvals$iter, df_pvals$look), ])
}

# power calculation
# would be eventually generalizable for any outcome
# for now only works with the variable names above
get_pow = function(p_values, alpha = 0.05) {
  msamp = max(p_values$n)
  cat(
    '-- FIXED DESIGN\nN(total) = ',
    msamp * 2,
    '\nType I error: ',
    mean(p_values$p_h0 < alpha),
    '\nPower: ',
    mean(p_values$p_h1[p_values$n == msamp] < alpha),
    ' (cf. via pwr: ',
    round(pwr::pwr.t.test(
      n = msamp, sig.level = alpha, d = .5
    )$power, 5),
    ')\n',
    sep = ''
  )

  looks = unique(p_values$look)

  # df_pow = list()
  # a_name = paste0('a_', as.character(round(a_adj * 10000)))

  # The "trial and error" straircase procedure below, for getting the desired adjusted alpha, seems cumbersome and possibly unnecessary, but I can't think of a better way
  a_adj = alpha / length(looks)
  a_step = -0.01
  while (abs(a_step) > 0.000002) {
    p_values$h0_sign = p_values$p_h0 < a_adj
    type1 = mean(aggregate(h0_sign ~ iter, data = p_values, FUN = any)$h0_sign)
    if (round(type1, 5) == round(alpha, 5)) {
      break
    } else if ((type1 < alpha &&
                a_step < 0) || (type1 > alpha && a_step > 0)) {
      a_step = -a_step / 2
    }
    a_adj = a_adj + a_step
    # cat('type1', type1, 'a_adj:', a_adj, 'new step:', a_step, fill = T)
  }
  p_values$h1_sign = p_values$p_h1 < a_adj
  seq_power = mean(aggregate(h1_sign ~ iter, data = p_values, FUN = any)$h1_sign)

  ps_sub0 = p_values
  ps_sub1 = p_values
  iters_tot = length(unique(p_values$iter))
  stops = list()
  for (lk in looks) {
    iters_out0 = ps_sub0$iter[ps_sub0$look == lk & ps_sub0$p_h0 < a_adj]
    ps_sub0 = ps_sub0[!ps_sub0$iter %in% iters_out0, ]
    iters_out1 = ps_sub1$iter[ps_sub1$look == lk &
                                ps_sub1$p_h1 < a_adj]
    ps_sub1 = ps_sub1[!ps_sub1$iter %in% iters_out1, ]

    stops[[length(stops) + 1]] = c(
      look = lk,
      ratio_h0 = length(iters_out0) / iters_tot,
      ratio_h1 = length(iters_out1) / iters_tot
    )
  }
  stops[[length(stops) + 1]]  = c(
    look = -1,
    ratio_h0 = nrow(ps_sub0) / iters_tot / length(looks),
    ratio_h1 = nrow(ps_sub1) / iters_tot / length(looks)
  )
  df_stops = as.data.frame(do.call(rbind, stops))
  df_stops$samples = c(unique(p_values$n), msamp)
  df_stops$avg_0 = df_stops$ratio_h0 * df_stops$samples
  df_stops$avg_1 = df_stops$ratio_h1 * df_stops$samples
  df_stops = rbind(df_stops, colSums(df_stops))
  dflen = nrow(df_stops)
  df_stops$look[dflen - 1] = 'remains'
  df_stops$look[dflen] = 'totals'

  cat(
    '\n-- SEQUENTIAL DESIGN\nN(total-avg) = ',
    df_stops$avg_0[dflen] * 2,
    ' (if H0 true) or ',
    df_stops$avg_1[dflen] * 2,
    ' (if H1 true)\nType I error: ',
    type1,
    '\nAdjusted (constant) local alpha: ',
    as.character(round(a_adj, 5)),
    '\nPower: ',
    seq_power,
    '\n\n',
    sep = ''
  )
  print(df_stops)
}


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
  p_val0 = t.test(sampl$v1a_n, sampl$v1b_n, var.equal = T)$p.value
  p_val1 = t.test(sampl$v1a_n, sampl$v2_n, var.equal = T)$p.value
  return(c(p_h0 = p_val0, p_h1 = p_val1))
}

# set.seed(2021) # you could do set.seed to get exactly what I got
# but probably doesn't matter here

# run simulation
df_ps = sim_pvals(custom_sample, custom_test, list(n = c(30, 60, 90)), 10000)

# get power for conventional alpha
get_pow(df_ps, alpha = .05)

# again for small alpha
get_pow(df_ps, alpha = .001)

# at the moment it's probably senselessly precise
get_pow(df_ps, alpha = .1735)


# TODO:

# Add futility bounds (based on beta? -- not yet sure about the way of implementation)

# Allow to specify not only overall alpha but also the ratio of interim nominal alphas (i.e., if not Pocock; e.g., last one largest if they prioritize informative study over efficiency -- maybe decided by established correction approaches as in rpact, but this may be beyond the scope)

# Allow for multiple testing correction, where each p value is provided in the function (e.g. as a named vector pvalues = c(maineffect1 = .34, maineffect2 = .66, interacteff = .49) ), and optional choice for correction (argument passed to p.adjust()). (But then it must be somehow specified to which p value(s) does the given stopping boundary apply.) Of course, custom correction can be given within the user-defined function too.

# Allow specifying varying factors (in particular: more than one effect size for H1, but also, e.g., different correlations of data) that will then be saved and summarized separately. (These factors could be passed to the function e.g. as "..." given each of their values in each simulation (extracted as formals(myfunc)).)

# In simulation loop record not only p values but also custom data (e.g. effect size), to be able to see how they vary

# Use loading bar if available ("suggest" via R package)

# Add illustrative comparison of Bonferroni corrections

