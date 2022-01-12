# A first sketch for Power Simulation for Sequential Analysis

# simulation procedure to get p values
sim_pvals = function(f_sample, n_obs, f_test, n_iter = 1000) {
  if (is.atomic(n_obs)) {
    # if vector given, all samples equal
    n_obs_orig = n_obs
    n_obs = list()
    for (n_name in formalArgs(f_sample)) {
      n_obs[[n_name]] = n_obs_orig
    }
  }
  n_look = length(n_obs[[1]])
  if (!all(sapply(n_obs, length) == n_look)) {
    stop('The lengths of the "n_obs" values are unequal.')
  }
  obs_per_it = do.call(Map, c(f = list, n_obs)) # transpose for iteration below
  list_vals = list()
  pb = utils::txtProgressBar(
    min = 0,
    max = n_iter,
    initial = 0,
    style = 3
  )
  obs_names = names(obs_per_it[[1]])
  for (i in 1:n_iter) {
    setTxtProgressBar(pb, i)
    samples = do.call(f_sample, obs_per_it[[n_look]])
    list_vals[[length(list_vals) + 1]] =
      c(iter = i,
        look = n_look,
        unlist(obs_per_it[[n_look]]),
        f_test(samples))
    for (lk in (n_look - 1):1) {
      for (samp_n in obs_names) {
        if (endsWith(samp_n, '_h')) {
          for (h_num in c('0', '1')) {
            samples[[paste0(samp_n, h_num)]] = sample(samples[[paste0(samp_n, h_num)]],
                                                      obs_per_it[[lk]][[samp_n]])
          }
        } else {
          samples[[samp_n]] = sample(samples[[samp_n]],
                                     obs_per_it[[lk]][[samp_n]])
        }
      }
      list_vals[[length(list_vals) + 1]] =
        c(iter = i,
          look = lk,
          unlist(obs_per_it[[lk]]),
          f_test(samples))
    }
  }
  close(pb)
  df_pvals = as.data.frame(do.call(rbind, list_vals))
  df_pvals = df_pvals[order(df_pvals$iter, df_pvals$look),]
  for (c_nam in names(n_obs)) {
    class(df_pvals[[c_nam]]) = c(class(df_pvals[[c_nam]]), "possa_n")
  }
  class(df_pvals) = c(class(df_pvals), "possa_df")
  return(df_pvals)
}

# power calculation
get_pow = function(p_values, alpha = 0.05) {
  n_cols = c()
  for (c_nam in colnames(p_values)) {
    col = p_values[[c_nam]]
    if ('possa_n' %in% class(col)) {
      n_cols = c(n_cols, c_nam)
    }
  }
  mlook = max(p_values$look)
  row1 = p_values[p_values$look == mlook,][1,]
  msamp = sum(row1[n_cols])

  cat(
    '-- FIXED DESIGN\nN(total) = ',
    msamp * 2,
    '\nType I error: ',
    mean(p_values$p_h0 < alpha),
    '\nPower: ',
    mean(p_values$p_h1[p_values$look == mlook] < alpha),
    ' (cf. via pwr: ',
    round(pwr::pwr.t.test(
      n = msamp, sig.level = alpha, d = .5
    )$power, 5),
    ')\n\n',
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
    cat('\r| Getting alpha: ', type1, ' (step: ', a_step, ') |    ', sep = '')
    flush.console()
  }
  cat('\r                                                                     ')
  flush.console()
  p_values$h1_sign = p_values$p_h1 < a_adj
  seq_power = mean(aggregate(h1_sign ~ iter, data = p_values, FUN = any)$h1_sign)

  ps_sub0 = p_values
  ps_sub1 = p_values
  iters_tot = length(unique(p_values$iter))
  stops = list()
  for (lk in looks) {
    iters_out0 = ps_sub0$iter[ps_sub0$look == lk & ps_sub0$p_h0 < a_adj]
    ps_sub0 = ps_sub0[!ps_sub0$iter %in% iters_out0,]
    iters_out1 = ps_sub1$iter[ps_sub1$look == lk &
                                ps_sub1$p_h1 < a_adj]
    ps_sub1 = ps_sub1[!ps_sub1$iter %in% iters_out1,]

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
    '-- SEQUENTIAL DESIGN\nN(total-avg) = ',
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

library('gsDesign')
gsDesign::gsDesign(k = 3, sfu = sfLDPocock, test.type = 2)
gsDesign::gsDesign(k = 3, sfu = sfLDOF, test.type = 2)

alpha = 0.025
Kstops = 3
z_total = qnorm(1 - alpha)
for (k_stop in 1:Kstops) {
  z_interim = z_total * (sqrt(Kstops / k_stop))
  a_interim = 1 - pnorm(z_interim)
  a_frac = k_stop / Kstops
  a_interim_OFf = 4 * (1 - pnorm(qnorm(1 - (alpha / 4)) / (sqrt(a_frac))))
  a_interim_Pf = alpha * log(1 + (exp(1) - 1) * a_frac)
  message(
    'Look #',
    k_stop,
    '\nO Brien and Fleming alpha: ',
    round(a_interim, 5),
    ', z: ',
    round(z_interim, 5),
    '\nO Brien and Fleming alpha with function: ',
    round(a_interim_OFf, 5),
    ', z: ',
    round(qnorm(1 - a_interim_OFf), 5),
    '\nPockock alpha with function: ',
    round(a_interim_Pf, 5),
    ', z: ',
    round(qnorm(1 - a_interim_Pf), 5)
  )
}

# user-defined function to specify sample(s)
custom_sample =  function(v1, v2_h) {
  samples = list()
  samples$v1 = rnorm(v1, mean = 0, sd = 1)
  samples$v2_h0 = rnorm(v2_h, mean = 0, sd = 1)
  samples$v2_h1 = rnorm(v2_h, mean = 0.5, sd = 1)
  return(samples)
}


# user-defined function to specify significance test(s)
custom_test = function(sampl) {
  p_val0 = t.test(sampl$v1, sampl$v2_h0, var.equal = T)$p.value
  p_val1 = t.test(sampl$v1, sampl$v2_h1, var.equal = T)$p.value
  return(c(p_h0 = p_val0, p_h1 = p_val1))
}

# set.seed(2022)

# run simulation
df_ps = sim_pvals(
  f_sample = custom_sample,
  n_obs = c(30, 60, 90),
  f_test = custom_test,
  1000
)
df_ps = sim_pvals(
  f_sample = custom_sample,
  n_obs = list(v1 = c(30, 60, 90),
               v2_h = c(30, 60, 90)),
  f_test = custom_test,
  10000
)

# get power for conventional alpha
get_pow(df_ps, alpha = .05)

# again for small alpha
get_pow(df_ps, alpha = .001)

# at the moment it's probably senselessly precise
get_pow(df_ps, alpha = .1735)
