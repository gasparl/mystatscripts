# A first sketch for Power Simulation for Sequential Analysis

rowise =  function(datcols, func) {
  return(apply(do.call(cbind, args = datcols), 1, func))
}

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
                   fut_locals = NULL,
                   group_by = NULL,
                   descr_cols = TRUE,
                   descr_func = summary,
                   round_to = 3,
                   multi_logic = all,
                   multi_logic_fut = all,
                   staircase_steps = 0.01 * (0.5 ** (seq(0, 11, 1))),
                   # default: 11 steps from 0.01, decreasing by halves
                   # check: formatC(staircase_steps, digits = 12, format = "f")
                   alpha_precision = 5) {
  if (!'possa_df' %in% class(p_values)) {
    warning(
      'The given data frame seems not to have been created by the "possa::sim_pvals()" function; it may not fit the "possa::get_pow()" function.',
      immediate. = TRUE
    )
  }
  looks = unique(p_values$look)
  mlook = max(p_values$look)
  # get columns with sample sizes (n), factors (fac), and p values (p_/_h0/1)
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
  a_locals = list()
  # extract (if given) predetermined local alphas and/or specified p value columns
  if (!is.null(alpha_locals)) {
    if (is.atomic(alpha_locals)) {
      # if vector given
      if (is.numeric(alpha_locals)) {
        # if numeric, assign to each p column
        if (!length(alpha_locals) == mlook) {
          stop(
            'Wrong argument for "alpha_locals". (If a numeric vector is given, ',
            'it must have same length as the maximum number of looks (in this case ',
            mlook,
            ').)'
          )
        }
        for (pnam in p_names) {
          a_locals[[pnam]] = alpha_locals
        }
        loc_pnames = p_names
      } else {
        # if character, simpy give the names
        loc_pnames = alpha_locals
      }
    } else {
      # if list, assign per name
      for (a_vec in alpha_locals) {
        if (!(is.atomic(a_vec) && length(a_vec) == mlook)) {
          stop(
            'Wrong argument for "alpha_locals". (If a list is given, ',
            'it must consist of one or more vectors of numbers with the',
            ' same length as the maximum number of looks (in this case ',
            mlook,
            ').)'
          )
        }
        a_locals = alpha_locals
      }
      loc_pnames = names(alpha_locals)
    }
    for (pname in loc_pnames) {
      if (!pname %in% p_names) {
        stop(
          'Wrong argument for "alpha_locals". ',
          'There is no column name pair "',
          pname,
          '0"/"',
          pname,
          '1".'
        )
      }
    }
    p_names = loc_pnames
  }
  # if not given, add NA for all local alphas
  if (!length(a_locals) > 0) {
    for (pnam in p_names) {
      a_locals[[pnam]] = rep(NA, mlook)
    }
  }
  fa_locals = list()
  # extract (if given) predetermined futility bounds
  if (!is.null(fut_locals)) {
    if (is.atomic(fut_locals)) {
      # if vector given, assign to each p column
      if (!length(fut_locals) == mlook) {
        stop(
          'Wrong argument for "fut_locals". (If a numeric vector is given, ',
          'it must have same length as the maximum number of looks (in this case ',
          mlook,
          ').)'
        )
      }
      for (pnam in p_names) {
        fa_locals[[pnam]] = fut_locals
      }
    } else {
      # if list, assign per name
      for (a_vec in fut_locals) {
        if (!(is.atomic(a_vec) && length(a_vec) == mlook)) {
          stop(
            'Wrong argument for "fut_locals". (If a list is given, ',
            'it must consist of one or more vectors of numbers with the',
            ' same length as the maximum number of looks (in this case ',
            mlook,
            ').)'
          )
        }
        fa_locals = fut_locals
      }
      for (pname in names(fut_locals)) {
        if (!pname %in% p_names) {
          stop(
            'Wrong argument for "fut_locals". ',
            'There is no column name pair "',
            pname,
            '0"/"',
            pname,
            '1".'
          )
        }
      }
    }
  }
  # if not given, add 1 for all local futility bounds
  if (!length(fa_locals) > 0) {
    for (pnam in p_names) {
      fa_locals[[pnam]] = rep(1, mlook)
    }
  }
  if (is.null(group_by)) {
    group_by = fac_cols
  } else if (!identical(sort(group_by), sort(fac_cols))) {
    message('Custom "group_by" argument given. Be cautious.')
  }
  if (length(group_by) > 0) {
    p_values$possa_facts_combS = do.call(paste, c(p_values[group_by], sep =
                                                    '; '))
    possafacts = unique(p_values$possa_facts_combS)
  } else {
    possafacts = NA
  }
  p_names_h0 = paste0(p_names, '_h0')
  p_names_h1 = paste0(p_names, '_h1')
  if (descr_cols[1] == TRUE) {
    descr_cols = names(p_values)[!names(p_values) %in%
                                   c('iter', 'look', p_names_h0, p_names_h1, n_cols, fac_cols)]
  }
  out_dfs = list()
  # calculate results separately for each factor combination
  for (possa_fact in possafacts) {
    if (is.na(possafacts)) {
      pvals_df = p_values
    } else {
      # print descriptives of all included
      if (descr_cols[1] != FALSE && possa_fact == possafacts[1]) {
        cat('-- DESCRIPTIVES (total) --', fill = TRUE)
        for (desc_col in descr_cols) {
          cat(desc_col, ': ', sep = '')
          descr_func(p_values[[desc_col]])
        }
      }
      # if applicable, take only given factor combination & print its "group" name
      pvals_df = p_values[p_values$possa_facts_combS == possa_fact, ]
      cat('GROUP: ', possa_fact, fill = TRUE)
    }
    if (descr_cols[1] != FALSE) {
      cat('-- DESCRIPTIVES --', fill = TRUE)
      for (desc_col in descr_cols) {
        cat(desc_col, ': ', sep = '', fill = TRUE)
        print(descr_func(pvals_df[[desc_col]]))
      }
    }
    tot_samples = c()
    for (lk in looks) {
      tot_samples = c(tot_samples, sum(pvals_df[pvals_df$look == lk,][1,][n_cols]))
    }
    pvals_df_fix = pvals_df[pvals_df$look == mlook, ]
    cat('-- FIXED DESIGN; N(total) =',
        tot_samples[length(tot_samples)], '--', fill = TRUE)
    for (p_nam in p_names) {
      cat(
        '(',
        p_nam,
        ') Type I error: ',
        round(mean(pvals_df_fix[[paste0(p_nam, '_h0')]] < alpha_global), round_to),
        '; Power: ',
        round(mean(pvals_df_fix[[paste0(p_nam, '_h1')]] < alpha_global), round_to),
        '\n\n',
        sep = '',
        fill = TRUE
      )
    }

    # the "trial and error" straircase procedure below, for getting the desired adjusted alpha, seems cumbersome and possibly unnecessary, but I can't think of a better way
    locls_temp = a_locals
    a_adj = alpha_global / length(looks) # start with bonferroni
    strair_steps = staircase_steps
    a_step = -strair_steps[1] # initial alpha-decreasing step
    strair_steps = strair_steps[-1]
    p_h0_sign_names = paste0(p_names, '_h0_sign')
    p_h0_fut_names = paste0(p_names, '_h0_fut')
    p_h1_sign_names = paste0(p_names, '_h1_sign')
    p_h1_fut_names = paste0(p_names, '_h1_fut')
    pb = utils::txtProgressBar(
      min = 0,
      max = length(strair_steps),
      initial = 0,
      style = 3
    )
    while (length(strair_steps) > 0) {
      # calculate H0 significances (T/F) based on adjusted alpha
      for (p_nam in p_names) {
        locls_temp[[p_nam]][is.na(a_locals[[p_nam]])] = a_adj # adjust alpha where missing
        pvals_df[[paste0(p_nam, '_h0_sign')]] = NA # create sign column for given p
        if (!is.null(fut_locals)) {
          # if futility bounds are given
          pvals_df[[paste0(p_nam, '_h0_fut')]] = NA # create fut column for given p
        }
        for (lk in 1:mlook) {
          # decide significance at given look for given p
          pvals_df[[paste0(p_nam, '_h0_sign')]][pvals_df$look == lk] =
            pvals_df[[paste0(p_nam, '_h0')]][pvals_df$look == lk] < locls_temp[[p_nam]][lk]
          if (!is.null(fut_locals)) {
            pvals_df[[paste0(p_nam, '_h0_fut')]][pvals_df$look == lk] =
              pvals_df[[paste0(p_nam, '_h0')]][pvals_df$look == lk] > fa_locals[[p_nam]][lk]
          }
        }
      }
      # now check the global type 1 error
      # if multiple p columns, check at which look we stop
      pvals_df$h0_stoP = rowise(pvals_df[c(p_h0_sign_names)], multi_logic)
      if (!is.null(fut_locals)) {
        pvals_df$h0_stoP_fa = multi_logic(pvals_df[c(p_h0_fut_names)], multi_logic_fut)
        pvals_df$h0_stoP = any(pvals_df$h0_stoP, pvals_df$h0_stoP_fa)
      }
      pvals_df_stp = pvals_df[pvals_df$h0_stoP == TRUE |
                                pvals_df$look == mlook, ]
      pvals_df_stp = do.call(rbind, lapply(split(
        pvals_df_stp, as.factor(pvals_df_stp$iter)
      ), function(x) {
        return(x[which.min(x$look),])
      }))
      # now get all outcomes at stopping point
      type1 = mean(unlist(pvals_df_stp[p_h0_sign_names], use.names = FALSE))

      # break if global alpha is correct
      # otherwise continue the staircase
      if (round(type1, alpha_precision) == round(alpha_global, alpha_precision)) {
        break
      } else if ((type1 < alpha_global &&
                  a_step < 0) ||
                 (type1 > alpha_global && a_step > 0)) {
        # change staircase direction (and also decrease step) if needed
        a_step = -strair_steps[1]
        strair_steps = strair_steps[-1]
        setTxtProgressBar(pb, getTxtProgressBar(pb) + 1)
      }
      a_adj = a_adj + a_step
      cat('type1', type1, 'a_adj:', a_adj, 'new step:', a_step, fill = T)
    }
    close(pb)
    a_locals = locls_temp
    # calculate H1 significances (T/F) based on adjusted alpha
    for (p_nam in p_names) {
      pvals_df[[paste0(p_nam, '_h1_sign')]] = NA
      if (!is.null(fut_locals)) {
        pvals_df[[paste0(p_nam, '_h1_fut')]] = NA
      }
      for (lk in 1:mlook) {
        pvals_df[[paste0(p_nam, '_h1_sign')]][pvals_df$look == lk] =
          pvals_df[[paste0(p_nam, '_h1')]][pvals_df$look == lk] < a_locals[[p_nam]][lk]
        if (!is.null(fut_locals)) {
          pvals_df[[paste0(p_nam, '_h1_fut')]][pvals_df$look == lk] =
            pvals_df[[paste0(p_nam, '_h1')]][pvals_df$look == lk] > fa_locals[[p_nam]][lk]
        }
      }
    }
    # now check the power
    # if multiple p columns, check at which look we stop

    pvals_df$h1_stoP = rowise(pvals_df[c(p_h1_sign_names)], multi_logic)
    if (!is.null(fut_locals)) {
      pvals_df$h1_stoP_fa = rowise(pvals_df[c(p_h1_fut_names)], multi_logic_fut)
      pvals_df$h1_stoP = any(pvals_df$h1_stoP, pvals_df$h1_stoP_fa)
    }
    pvals_df_stp = pvals_df[pvals_df$h1_stoP == TRUE |
                              pvals_df$look == mlook, ]
    pvals_df_stp = do.call(rbind, lapply(split(
      pvals_df_stp, as.factor(pvals_df_stp$iter)
    ), function(x) {
      return(x[which.min(x$look),])
    }))
    # now get all outcomes at stopping point
    seq_power = mean(unlist(pvals_df_stp[p_h1_sign_names], use.names = FALSE))

    # calculate sample size information per look
    ps_sub0 = pvals_df
    ps_sub1 = pvals_df
    iters_tot = length(unique(pvals_df$iter))
    stops = list()
    previous_h0 = iters_tot
    previous_h1 = iters_tot
    for (lk in looks) {
      iters_out0 = ps_sub0[ps_sub0$look == lk &
                             ps_sub0$h0_stoP == TRUE, ]
      ps_sub0 = ps_sub0[!ps_sub0$iter %in% iters_out0$iter,]
      iters_out1 = ps_sub1[ps_sub1$look == lk &
                             ps_sub1$h1_stoP == TRUE, ]
      ps_sub1 = ps_sub1[!ps_sub1$iter %in% iters_out1$iter, ]
      outs = c()
      for (p_nam in p_names) {
        outs[paste0('iters_sign_', p_nam, '_h0')] =
          sum(iters_out0[[paste0(p_nam, '_h0_sign')]])
        outs[paste0('iters_sign_', p_nam, '_h1')] =
          sum(iters_out1[[paste0(p_nam, '_h1_sign')]])
        if (!is.null(fut_locals)) {
          outs[paste0('iters_futil_', p_nam, '_h0')] =
            sum(iters_out1[[paste0(p_nam, '_h0_fut')]])
          outs[paste0('iters_futil_', p_nam, '_h1')] =
            sum(iters_out1[[paste0(p_nam, '_h1_fut')]])
        }
      }
      outs['iters_remain_h0'] =
        length(unique(ps_sub0$iter))
      outs['iters_remain_h1'] =
        length(unique(ps_sub1$iter))
      if (lk == mlook) {
        # at last look, all is stopped that previously remained
        outs['iters_stopped_h0'] = previous_h0
        outs['iters_stopped_h1'] = previous_h1
      } else {
        outs['iters_stopped_h0'] = previous_h0 - outs['iters_remain_h0']
        outs['iters_stopped_h1'] = previous_h1 - outs['iters_remain_h1']
      }
      stops[[length(stops) + 1]] = c(look = lk,
                                     sample = tot_samples[lk],
                                     outs)
      previous_h0 = outs['iters_remain_h0']
      previous_h1 = outs['iters_remain_h1']
    }
    df_stops = as.data.frame(do.call(rbind, stops))

    # df_stops = df_stps # TO REMOVE

    for (pnam in p_names) {
      df_stops[paste0('alpha_local_', p_nam)] = a_locals[[pnam]]
      if (!is.null(fut_locals)) {
        df_stops[paste0('futil_local_', p_nam)] = fa_locals[[pnam]]
      }
      for (h01 in c('_h0', '_h1')) {
        df_stops[paste0('ratio_sign_', p_nam, h01)] = df_stops[paste0('iters_sign_', p_nam, h01)] / iters_tot
        if (!is.null(fut_locals)) {
          df_stops[paste0('ratio_futil_', p_nam, h01)] = df_stops[paste0('iters_futil_', p_nam, h01)] / iters_tot
        }
      }
    }
    df_stops$ratio_stopped_h0 = df_stops$iters_stopped_h0 / iters_tot
    df_stops$ratio_stopped_h1 = df_stops$iters_stopped_h1 / iters_tot
    df_stops$ratio_remain_h0 = df_stops$iters_remain_h0 / iters_tot
    df_stops$ratio_remain_h1 = df_stops$iters_remain_h1 / iters_tot
    # the average sample proportion at the given look
    df_stops$samp_avg_prop_0 = df_stops$ratio_stopped_h0 * df_stops$sample
    df_stops$samp_avg_prop_1 = df_stops$ratio_stopped_h1 * df_stops$sample

    df_stops = rbind(df_stops, colSums(df_stops))
    df_stops$look[nrow(df_stops)] = 'totals'

    ############################################################
    # somehow calculate type 1 & power

    ### PER GROUND TRUTH (H0 & H1) & PER P VALUE (P-NAME)
    # overall type 1 error
    # overall power


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
get_pow(df_ps)

# again for small alpha
get_pow(df_ps, alpha_global = .001)

# at the moment it's probably senselessly precise
get_pow(df_ps, alpha_global = .1735)
