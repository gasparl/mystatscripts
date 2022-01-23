# A first sketch for Power Simulation for Sequential Analysis

# simulation procedure to get p values
sim_pvals = function(f_sample,
                     n_obs,
                     f_test,
                     n_iter = 1000,
                     seed = 8) {
  set.seed(seed)
  if (!is.function(f_sample)) {
    f_s_args = f_sample
    if (!is.function(f_s_args[[1]])) {
      stop('When "f_sample" is given as list,',
           ' the first argument must be a function.')
    }
    f_sample = f_s_args[[1]]
    f_s_args[[1]] = NULL
    # get all f_sample argument combinations
    df_combs = sapply(expand.grid(f_s_args), as.vector)
    f_s_a_list = list()
    for (rownum in 1:nrow(df_combs)) {
      f_s_a_list[[rownum]] = as.list(df_combs[rownum, ])
    }
  } else {
    f_s_a_list = NA
    f_s_args = c()
  }
  if (is.atomic(n_obs)) {
    # if vector given, all samples equal
    n_obs_orig = n_obs
    n_obs = list()
    fpars = formalArgs(f_sample)
    for (n_name in fpars[!fpars %in% names(f_s_args)]) {
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
    max = n_iter * length(f_s_a_list),
    initial = 0,
    style = 3
  )
  obs_names = names(obs_per_it[[1]])
  pb_count = 0
  for (f_s_a in f_s_a_list) {
    if (is.na(f_s_a[1])) {
      f_s_a = NULL
    }
    for (i in 1:n_iter) {
      pb_count = pb_count + 1
      setTxtProgressBar(pb, pb_count)
      samples = do.call(f_sample, c(obs_per_it[[n_look]], f_s_a))
      list_vals[[length(list_vals) + 1]] =
        c(
          iter = i,
          look = n_look,
          unlist(f_s_a),
          unlist(obs_per_it[[n_look]]),
          f_test(samples)
        )
      for (lk in (n_look - 1):1) {
        seed = .Random.seed
        for (samp_n in obs_names) {
          if (endsWith(samp_n, '_h')) {
            for (h_num in c('0', '1')) {
              .Random.seed = seed
              samples[[paste0(samp_n, h_num)]] = sample(samples[[paste0(samp_n, h_num)]],
                                                        obs_per_it[[lk]][[samp_n]])
            }
          } else {
            .Random.seed = seed
            samples[[samp_n]] = sample(samples[[samp_n]],
                                       obs_per_it[[lk]][[samp_n]])
          }
        }
        list_vals[[length(list_vals) + 1]] =
          c(
            iter = i,
            look = lk,
            unlist(f_s_a),
            unlist(obs_per_it[[lk]]),
            f_test(samples)
          )
      }
    }
  }
  close(pb)
  df_pvals = as.data.frame(do.call(rbind, list_vals))
  df_pvals = df_pvals[order(df_pvals$iter, df_pvals$look), ]
  for (c_nam in names(n_obs)) {
    class(df_pvals[[c_nam]]) = c(class(df_pvals[[c_nam]]), "possa_n")
  }
  for (fc_nam in names(f_s_args)) {
    class(df_pvals[[fc_nam]]) = c(class(df_pvals[[fc_nam]]), "possa_fac")
  }
  class(df_pvals) = c(class(df_pvals), "possa_df")
  return(df_pvals)
}

# power calculation
get_pow = function(p_values,
                   alpha_locals = NULL,
                   alpha_global = 0.05,
                   group_by = NULL,
                   round_to = 3) {
  if ('possa_df' %in% class(p_values)) {
    warning(
      'The given data frame seems not to have been created by the "possa::sim_pvals()" function; it may not fit the "possa::get_pow()" function.',
      immediate. = TRUE
    )
  }
  n_cols = c()
  fac_cols = c()
  p_names = c()
  for (c_nam in colnames(p_values)) {
    col = p_values[[c_nam]]
    if ('possa_n' %in% class(col)) {
      n_cols = c(n_cols, c_nam)
    } else if ('possa_fac' %in% class(col)) {
      fac_cols = c(fac_cols, c_nam)
    } else if (startsWith(c_nam, 'p_') && endsWith(c_nam, '_h0')) {
      pnam = substr(c_nam, 1, nchar(c_nam) - 3)
      if (paste0(pnam, '_h1') %in% colnames(p_values)) {
        p_names = c(p_names, pnam)
      }
    }
  }
  if (!is.null(alpha_locals)) {
    if (is.atomic(n_obs)) {
      loc_pnames = alpha_locals
    } else {
      loc_pnames = names(alpha_locals)
    }
    for (pname in loc_pnames) {
      if (!pname %in% p_names) {
        stop('There is no column name pair "',
             pname,
             '0"/"',
             pname,
             '1".')
      }
    }
    p_names = loc_pnames
  }
  if (is.null(group_by)) {
    group_by = fac_cols
  } else if (!identical(sort(group_by), sort(fac_cols))) {
    message('Custom "group_by" argument given. Be cautious.')
  }
  if ((!is.null(group_by)) & length(group_by) > 0) {
    p_values$possa_facts_combS = do.call(paste, c(p_values[group_by], sep =
                                                    '; '))
    possafacts = unique(p_values$possa_facts_combS)
  } else {
    possafacts = NA
  }
  out_dfs = list()
  for (possa_fact in possafacts) {
    if (is.na(possafacts)) {
      pvals_df = p_values
    } else {
      pvals_df = p_values[p_values$possa_facts_combS == possa_fact, ]
      cat('Group: ', possa_fact, fill = TRUE)
    }
    mlook = max(pvals_df$look)
    row1 = pvals_df[pvals_df$look == mlook, ][1, ]
    msamp = sum(row1[n_cols])

    pvals_df_fix = pvals_df[pvals_df$look == mlook, ]
    cat('-- FIXED DESIGN; N(total) =',
        msamp, '--', fill = TRUE)
    for (p_nam in p_names) {
      cat(
        '(',
        p_nam,
        ') Type I error: ',
        round(mean(pvals_df_fix[[paste0(p_nam, '_h0')]] < alpha), round_to),
        '; Power: ',
        round(mean(pvals_df_fix[[paste0(p_nam, '_h1')]] < alpha), round_to),
        '\n\n',
        sep = '',
        fill = TRUE
      )
    }

    # getting vector from cols: unlist(pvals_df[p_h0_names], use.names = FALSE)

    looks = unique(pvals_df$look)

    # df_pow = list()
    # a_name = paste0('a_', as.character(round(a_adj * 10000)))

    # The "trial and error" straircase procedure below, for getting the desired adjusted alpha, seems cumbersome and possibly unnecessary, but I can't think of a better way
    a_adj = alpha / length(looks)
    a_step = -0.01
    while (abs(a_step) > 0.000002) {
      pvals_df$h0_sign = pvals_df$p_h0 < a_adj
      type1 = mean(aggregate(h0_sign ~ iter, data = pvals_df, FUN = any)$h0_sign)
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
    pvals_df$h1_sign = pvals_df$p_h1 < a_adj
    seq_power = mean(aggregate(h1_sign ~ iter, data = pvals_df, FUN = any)$h1_sign)

    ps_sub0 = pvals_df
    ps_sub1 = pvals_df
    iters_tot = length(unique(pvals_df$iter))
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
    df_stops$samples = c(unique(pvals_df$n), msamp)
    df_stops$avg_0 = df_stops$ratio_h0 * df_stops$samples
    df_stops$avg_1 = df_stops$ratio_h1 * df_stops$samples
    df_stops = rbind(df_stops, colSums(df_stops))
    dflen = nrow(df_stops)
    df_stops$look[dflen - 1] = 'remains'
    df_stops$look[dflen] = 'totals'

    cat(
      '-- SEQUENTIAL DESIGN --\nN(total-avg) = ',
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
    out_dfs[[length(out_dfs) + 1]] = df_stops
  }
  if (is.na(possafacts)) {
    out_dfs = out_dfs[[1]]
  }
  invisible(out_dfs)
}


# user-defined function to specify sample(s)
custom_sample1 = function(v1, v2_h) {
  samples = list()
  samples$v1 = rnorm(v1, mean = 0, sd = 1)
  samples$v2_h0 = rnorm(v2_h, mean = 0, sd = 1)
  samples$v2_h1 = rnorm(v2_h, mean = 1, sd = 1)
  return(samples)
}

custom_sample2 = function(v1, v2_h, h1_mean, h1_sd) {
  samples = list()
  samples$v1 = rnorm(v1, mean = 0, sd = 1)
  samples$v2_h0 = rnorm(v2_h, mean = 0, sd = 1)
  samples$v2_h1 = rnorm(v2_h, mean = h1_mean, sd = h1_sd)
  return(samples)
}

# user-defined function to specify significance test(s)
custom_test = function(sampl) {
  t0 = t.test(sampl$v2_h0, sampl$v1, var.equal = T)
  the_smd0 = MBESS::ci.smd(
    ncp = t0$statistic,
    n.1 = length(sampl$v1),
    n.2 = length(sampl$v2_h0)
  )
  t1 = stats::t.test(sampl$v2_h1, sampl$v1, var.equal = T)
  the_smd1 = MBESS::ci.smd(
    ncp = t1$statistic,
    n.1 = length(sampl$v1),
    n.2 = length(sampl$v2_h1)
  )
  return(
    c(
      p_h0 = t0$p.value,
      p_h1 = t1$p.value,
      cohens_d_0 = as.numeric(the_smd0$smd),
      m_diff_0 = mean(sampl$v2_h0) - mean(sampl$v1),
      cohens_d_1 = as.numeric(the_smd1$smd),
      m_diff_1 = mean(sampl$v2_h1) - mean(sampl$v1)
    )
  )
}


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
  f_sample = custom_sample,
  n_obs = c(30, 60, 90),
  f_test = custom_test,
  n_iter = 1000
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
get_pow(df_ps, alpha = .05)

# again for small alpha
get_pow(df_ps, alpha = .001)

# at the moment it's probably senselessly precise
get_pow(df_ps, alpha = .1735)
