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
  # sanity checks for given values
  if (!is.function(f_sample)) {
    f_s_args = f_sample
    if (!is.function(f_s_args[[1]])) {
      stop('When "f_sample" is given as list,',
           ' the first argument must be a function.')
    }
    f_sample = f_s_args[[1]]
    f_s_args[[1]] = NULL
    # get all f_sample argument combinations (different sample types)
    df_combs = sapply(expand.grid(f_s_args), as.vector)
    f_s_a_list = list()
    for (rownum in 1:nrow(df_combs)) {
      f_s_a_list[[rownum]] = as.list(df_combs[rownum,])
    }
  } else {
    # set to have no combinations; single sample test (hence 1 cycle below)
    f_s_a_list = NA
    f_s_args = c()
  }
  if (is.atomic(n_obs)) {
    # if just a vector given, all samples have this as samples sizes
    n_obs_orig = n_obs
    n_obs = list()
    fpars = formalArgs(f_sample) # required sample names guessed from f_sample arguments
    for (n_name in fpars[!fpars %in% names(f_s_args)]) {
      n_obs[[n_name]] = n_obs_orig # assign vector to each sample type
    }
  }
  n_look = length(n_obs[[1]])
  if (!all(sapply(n_obs, length) == n_look)) {
    stop('The lengths of the "n_obs" values are unequal.')
  }
  obs_per_it = do.call(Map, c(f = list, n_obs)) # transpose input for iteration below
  obs_names = names(obs_per_it[[1]]) # the names of all sample columns
  list_vals = list() # all end results will be collected in this list
  # set progress bar
  pb = utils::txtProgressBar(
    min = 0,
    max = n_iter * length(f_s_a_list),
    initial = 0,
    style = 3
  )
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
  df_pvals = df_pvals[order(df_pvals$iter, df_pvals$look),]
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
                   alpha_locals_extra = NULL,
                   design_fix = NULL,
                   design_seq = TRUE,
                   descr_cols = TRUE,
                   descr_func = summary,
                   round_to = 3,
                   multi_logic = all,
                   multi_logic_fut = all,
                   staircase_steps = 0.01 * (0.5 ** (seq(0, 11, 1))),
                   # default: 11 steps from 0.01, decreasing by halves
                   # check: formatC(staircase_steps, digits = 12, format = "f")
                   alpha_precision = 5,
                   seed = 8) {
  set.seed(seed)
  if (!'possa_df' %in% class(p_values)) {
    warning(
      'The given data frame seems not to have been created by the "possa::sim_pvals()" function; it may not fit the "possa::get_pow()" function.',
      immediate. = TRUE
    )
  }
  if (any(is.na(staircase_steps))) {
    stop('The staircase_steps vector must not contain NA values.')
  }
  looks = unique(p_values$look)
  mlook = max(p_values$look)
  # get columns with sample sizes (n), factors (fac), and p values (p_/_h0/1)
  n_cols = c()
  fac_cols = c()
  p_names_auto = c()
  for (c_nam in colnames(p_values)) {
    col = p_values[[c_nam]]
    if ('possa_n' %in% class(col)) {
      n_cols = c(n_cols, c_nam)
    } else if ('possa_fac' %in% class(col)) {
      fac_cols = c(fac_cols, c_nam)
    } else if (startsWith(c_nam, 'p_') && endsWith(c_nam, '_h0')) {
      pnam = substr(c_nam, 1, nchar(c_nam) - 3)
      if (paste0(pnam, '_h1') %in% colnames(p_values)) {
        p_names_auto = c(p_names_auto, pnam)
      }
    }
  }
  a_locals = list()
  p_names = p_names_auto
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
        # if character, simply give the names
        loc_pnames = alpha_locals
      }
    } else {
      # if list, use as it is, and assign per name
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
      }
      a_locals = alpha_locals
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
  } else {
    lapply(a_locals, function(vec) {
      vec = vec[!is.na(vec)]
      if (any(vec > 1 | vec < 0)) {
        stop('All alpha values given in alpha_locals must be',
             ' between 0 and 1 (or NA).')
      }
    })
  }
  p_extr = NULL
  if (is.null(alpha_locals_extra)) {
    lapply(alpha_locals_extra, function(vec) {
      if (any(is.na(vec))) {
        stop('The alpha values given in alpha_locals_extra must ',
             'not contain NA values.')
      } else if (any(vec > 1 | vec < 0)) {
        stop('All alpha values given in alpha_locals_extra must be',
             ' between 0 and 1.')
      } else if (!(is.atomic(a_vec) && length(a_vec) == mlook)) {
        stop(
          'Wrong argument for "alpha_locals_extra". (If a list is given, ',
          'it must consist of one or more vectors of numbers with the',
          ' same length as the maximum number of looks (in this case ',
          mlook,
          ').)'
        )
      }
    })
    p_extr = names(alpha_locals_extra)
    for (pname in p_extr) {
      if (!pname %in% p_names_auto) {
        stop(
          'Wrong argument for "alpha_locals_extra". ',
          'There is no column name pair "',
          pname,
          '0"/"',
          pname,
          '1".'
        )
      }
    }
    a_locals = append(a_locals, alpha_locals_extra)
  }
  p_names_extr = c(p_names, p_extr)
  fa_locals = list()
  # extract (if given) predetermined futility bounds
  if (!is.null(fut_locals)) {
    if (is.atomic(fut_locals)) {
      # if vector given, assign to each p column
      if (!length(fut_locals) == (mlook - 1)) {
        stop(
          'Wrong argument for "fut_locals". (If a numeric vector is given, ',
          'its length must be one less than the maximum number of looks (in this case ',
          (mlook - 1),
          ').)'
        )
      }
      for (pnam in p_names) {
        fa_locals[[pnam]] = fut_locals
      }
    } else {
      # if list, assign per name
      for (a_vec in fut_locals) {
        if (!(is.atomic(a_vec) && length(a_vec) == (mlook-1))) {
          stop(
            'Wrong argument for "fut_locals". (If a list is given, ',
            'it must consist of one or more vectors of numbers with a ',
            'length one less than the maximum number of looks (in this case ',
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
  # if given, add 1 for the last look (since it should not matter)
  if (!length(fa_locals) > 0) {
    for (pnam in p_names) {
      fa_locals[[pnam]] = rep(1, mlook)
    }
  } else {
    for (pnam in p_names) {
      fa_locals[[pnam]] = c(fa_locals[[pnam]], 1)
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
                                   c(
                                     'iter',
                                     'look',
                                     paste0(p_names_extr, '_h0'),
                                     paste0(p_names_extr, '_h1'),
                                     n_cols,
                                     fac_cols
                                   )]
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
      pvals_df = p_values[p_values$possa_facts_combS == possa_fact,]
      cat('GROUP: ', possa_fact, fill = TRUE)
    }
    if (descr_cols[1] != FALSE) {
      cat('-- DESCRIPTIVES --', fill = TRUE)
      for (desc_col in descr_cols) {
        cat(desc_col, ': ', sep = '', fill = TRUE)
        print(descr_func(pvals_df[[desc_col]]))
      }
      cat('', fill = TRUE)
    }
    tot_samples = c()
    for (lk in looks) {
      tot_samples = c(tot_samples, sum(pvals_df[pvals_df$look == lk, ][1, ][n_cols]))
    }

    ## Fixed design calculation below
    if (is.null(design_fix)) {
      fix_looks = mlook # (default) show at max look only
    } else  if (design_fix == TRUE) {
      fix_looks = 1:mlook # show outcome at all looks
    } else if (design_fix == FALSE) {
      fix_looks = NULL # show none
    }
    for (f_look in fix_looks) {
      pvals_df_fix = pvals_df[pvals_df$look == f_look,]
      cat('-- FIXED DESIGN; N(total) =',
          tot_samples[f_look], '--', fill = TRUE)
      for (p_nam in p_names_extr) {
        cat(
          '(',
          p_nam,
          ') Type I error: ',
          round(mean(pvals_df_fix[[paste0(p_nam, '_h0')]] < alpha_global), round_to),
          '; Power: ',
          round(mean(pvals_df_fix[[paste0(p_nam, '_h1')]] < alpha_global), round_to),
          '\n',
          sep = '',
          fill = TRUE
        )
      }
    }


    ## Sequential design calculation below (when applicable)

    if (design_seq == TRUE & mlook > 1) {
      # "trial and error" straircase procedure below, to get the desired adjusted alpha
      locls_temp = a_locals
      a_adj = alpha_global / length(looks) # start with bonferroni
      stair_steps = staircase_steps
      if (!is.null(alpha_locals_extra)) {
        stair_steps = c(stair_steps, NA)
      }
      a_step = stair_steps[1] # initial alpha-changing step
      stair_steps = stair_steps[-1]
      p_h0_sign_names = paste0(p_names, '_h0_sign')
      p_h0_fut_names = paste0(p_names, '_h0_fut')
      p_h1_sign_names = paste0(p_names, '_h1_sign')
      p_h1_fut_names = paste0(p_names, '_h1_fut')
      pb = utils::txtProgressBar(
        min = 0,
        max = length(staircase_steps),
        initial = 0,
        style = 3
      )
      p_names_temp = p_names
      while (length(stair_steps) > 0) {
        # calculate H0 significances (T/F) & stops (T/F) based on adjusted alphas
        if (is.na(stair_steps[1])) {
          # as a last step, add non-stopping columns, if any
          p_names_temp = p_names_extr
        }
        for (p_nam in p_names_temp) {
          locls_temp[[p_nam]][is.na(a_locals[[p_nam]])] = a_adj # adjust alpha where missing
          pvals_df[[paste0(p_nam, '_h0_sign')]] = NA # create sign column for given p
          if (!is.null(fut_locals)) {
            # if futility bounds are given
            pvals_df[[paste0(p_nam, '_h0_fut')]] = TRUE # create fut column for given p
          }
          for (lk in 1:mlook) {
            # decide significance at given look for given p
            pvals_df[[paste0(p_nam, '_h0_sign')]][pvals_df$look == lk] =
              pvals_df[[paste0(p_nam, '_h0')]][pvals_df$look == lk] < locls_temp[[p_nam]][lk]
            if (!is.null(fut_locals) & lk != mlook) {
              pvals_df[[paste0(p_nam, '_h0_fut')]][pvals_df$look == lk] =
                pvals_df[[paste0(p_nam, '_h0')]][pvals_df$look == lk] > fa_locals[[p_nam]][lk]
            }
          }
        }
        # now check the global type 1 error
        # if multiple p columns, check at which look we stop
        if (!is.na(stair_steps[1])) {
          pvals_df$h0_stoP = rowise(pvals_df[c(p_h0_sign_names)], multi_logic)
          if (!is.null(fut_locals)) {
            pvals_df$h0_stoP_fa = multi_logic(pvals_df[c(p_h0_fut_names)], multi_logic_fut)
            pvals_df$h0_stoP = any(pvals_df$h0_stoP, pvals_df$h0_stoP_fa)
          }
          pvals_df_stp = pvals_df[pvals_df$h0_stoP == TRUE |
                                    pvals_df$look == mlook,]
          pvals_df_stp = do.call(rbind, lapply(split(
            pvals_df_stp, as.factor(pvals_df_stp$iter)
          ), function(x) {
            return(x[which.min(x$look), ])
          }))
          # now get all outcomes at stopping point
          type1 = mean(unlist(pvals_df_stp[p_h0_sign_names], use.names = FALSE))

          # break if global alpha is correct
          # otherwise continue the staircase
          if (round(type1, alpha_precision) == round(alpha_global, alpha_precision)) {
            if (is.null(alpha_locals_extra)) {
              break
            } else {
              stair_steps = 0
            }
          } else if ((type1 < alpha_global &&
                      a_step < 0) ||
                     (type1 > alpha_global && a_step > 0)) {
            # change staircase direction (and also decrease step) if needed
            a_step = -stair_steps[1] * sign(a_step)
            stair_steps = stair_steps[-1]
            setTxtProgressBar(pb, getTxtProgressBar(pb) + 1)
          }
          a_adj = a_adj + a_step

          ### TO REMOVE (just for testing)
          cat('\ntype1',
              type1,
              'a_adj:',
              a_adj,
              'new step:',
              a_step,
              fill = T)
        }
      }
      setTxtProgressBar(pb, length(staircase_steps))
      close(pb)
      # assign final local alphas
      a_locals_fin = locls_temp

      # calculate H1 significances (T/F) & stops (T/F) based on final alphas
      for (p_nam in p_names_extr) {
        pvals_df[[paste0(p_nam, '_h1_sign')]] = NA
        if (!is.null(fut_locals)) {
          pvals_df[[paste0(p_nam, '_h1_fut')]] = TRUE
        }
        for (lk in 1:mlook) {
          pvals_df[[paste0(p_nam, '_h1_sign')]][pvals_df$look == lk] =
            pvals_df[[paste0(p_nam, '_h1')]][pvals_df$look == lk] < a_locals_fin[[p_nam]][lk]
          if (!is.null(fut_locals) & lk != mlook) {
            pvals_df[[paste0(p_nam, '_h1_fut')]][pvals_df$look == lk] =
              pvals_df[[paste0(p_nam, '_h1')]][pvals_df$look == lk] > fa_locals_fin[[p_nam]][lk]
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
                                pvals_df$look == mlook,]
      pvals_df_stp = do.call(rbind, lapply(split(
        pvals_df_stp, as.factor(pvals_df_stp$iter)
      ), function(x) {
        return(x[which.min(x$look), ])
      }))
      # now get all outcomes at stopping point
      seq_power = mean(unlist(pvals_df_stp[p_h1_sign_names], use.names = FALSE))

      # calculate sample size information per look
      ps_sub0 = pvals_df
      ps_sub1 = pvals_df
      iters_tot = length(unique(pvals_df$iter))
      stops = list() # collect info per each stop
      previous_h0 = iters_tot # start with max for both
      previous_h1 = iters_tot
      for (lk in looks) {
        # get iterations stopped at given look
        iters_out0 = ps_sub0[ps_sub0$look == lk &
                               ps_sub0$h0_stoP == TRUE,]
        # remove stopped iterations
        ps_sub0 = ps_sub0[!ps_sub0$iter %in% iters_out0$iter, ]
        # (same for H1)
        iters_out1 = ps_sub1[ps_sub1$look == lk &
                               ps_sub1$h1_stoP == TRUE,]
        ps_sub1 = ps_sub1[!ps_sub1$iter %in% iters_out1$iter,]
        outs = c()
        # get info per p value column
        for (p_nam in p_names_extr) {
          # number of significant findings
          outs[paste0('iters_sign_', p_nam, '_h0')] =
            sum(iters_out0[[paste0(p_nam, '_h0_sign')]])
          outs[paste0('iters_sign_', p_nam, '_h1')] =
            sum(iters_out1[[paste0(p_nam, '_h1_sign')]])
          if (!is.null(fut_locals)) {
            # number of futility bound crossings
            outs[paste0('iters_futil_', p_nam, '_h0')] =
              sum(iters_out1[[paste0(p_nam, '_h0_fut')]])
            outs[paste0('iters_futil_', p_nam, '_h1')] =
              sum(iters_out1[[paste0(p_nam, '_h1_fut')]])
          }
        }
        # whatever remained
        outs['iters_remain_h0'] =
          length(unique(ps_sub0$iter))
        outs['iters_remain_h1'] =
          length(unique(ps_sub1$iter))
        if (lk == mlook) {
          # at last look, all is stopped that previously remained
          outs['iters_stopped_h0'] = previous_h0
          outs['iters_stopped_h1'] = previous_h1
        } else {
          # calculate stops as previous minus current remaining
          outs['iters_stopped_h0'] = previous_h0 - outs['iters_remain_h0']
          outs['iters_stopped_h1'] = previous_h1 - outs['iters_remain_h1']
        }
        stops[[length(stops) + 1]] = c(look = lk,
                                       sample = tot_samples[lk],
                                       outs)
        # assign current remaining as the next "previous remaining"
        previous_h0 = outs['iters_remain_h0']
        previous_h1 = outs['iters_remain_h1']
      }
      df_stops = as.data.frame(do.call(rbind, stops))
      # derive end info (type 1, power, etc) from the iteration ratios
      for (pnam in p_names_extr) {
        # write out local alphas
        df_stops[paste0('alpha_local_', p_nam)] = a_locals_fin[[pnam]]
        if (!is.null(fut_locals)) {
          # write out local futility bounds (if any)
          df_stops[paste0('futil_local_', p_nam)] = fa_locals[[pnam]]
        }
        for (h01 in c('_h0', '_h1')) {
          # ratio of significant findings at any given stop
          df_stops[paste0('ratio_sign_', p_nam, h01)] = df_stops[paste0('iters_sign_', p_nam, h01)] / iters_tot
          if (!is.null(fut_locals)) {
            # ratio of futility bound crossings at any given stop
            df_stops[paste0('ratio_futil_', p_nam, h01)] = df_stops[paste0('iters_futil_', p_nam, h01)] / iters_tot
          }
        }
      }
      # count ratios of stops per each look
      df_stops$ratio_stopped_h0 = df_stops$iters_stopped_h0 / iters_tot
      df_stops$ratio_stopped_h1 = df_stops$iters_stopped_h1 / iters_tot
      # count cumulative ratios of remainings
      df_stops$ratio_remain_h0 = df_stops$iters_remain_h0 / iters_tot
      df_stops$ratio_remain_h1 = df_stops$iters_remain_h1 / iters_tot
      # the average sample proportion at the given look (mainly just to calculate average-total)
      df_stops$samp_avg_prop_0 = df_stops$ratio_stopped_h0 * df_stops$sample
      df_stops$samp_avg_prop_1 = df_stops$ratio_stopped_h1 * df_stops$sample
      # calculate sums per each column (with different meaning in each case)
      df_stops = rbind(df_stops, colSums(df_stops))
      df_nrow = nrow(df_stops)
      df_stops$look[df_nrow] = 'totals'

      # print results for sequential design
      cat(
        '-- SEQUENTIAL DESIGN; N(average-total) = ',
        df_stops$samp_avg_prop_0[df_nrow],
        ' (if H0 true) or ',
        df_stops$samp_avg_prop_1[df_nrow],
        ' (if H1 true) --',
        sep = '',
        fill = TRUE
      )

      fut_text = ''
      for (p_nam in p_names_extr) {
        if (!is.null(fut_locals)) {
          fut_text = paste(paste0('(', looks, ') ',
                                  round(fa_locals[[pnam]][-mlook], round_to)), collapse = '; ')
        }
        cat(
          '(',
          p_nam,
          ') Type I error: ',
          round(df_stops[[paste0('ratio_sign_', p_nam, '_h0')]][df_nrow], round_to),
          '; Power: ',
          round(df_stops[[paste0('ratio_sign_', p_nam, '_h1')]][df_nrow], round_to),
          '\nAdjusted local alphas: ',
          paste(paste0(
            '(', looks, ') ', round(a_locals_fin[[pnam]], round_to)
          ), collapse = '; '),
          fut_text,
          '\n',
          sep = '',
          fill = TRUE
        )
      }
      out_dfs[[length(out_dfs) + 1]] = df_stops
    }
  }
  if (design_seq == TRUE & mlook > 1) {
    if (is.na(possafacts)) {
      out_dfs = out_dfs[[1]]
    }
    invisible(out_dfs)
  } else {
    invisible(NULL)
  }
}
