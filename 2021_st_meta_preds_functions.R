cohen_d_between = function(outcome, categ, for_table = T) {
  e = t.test(outcome ~ categ, var.equal = TRUE)
  f = as.vector(e$statistic)
  df = as.vector(e$parameter)
  pvalue = e$p.value
  table_categ = as.data.frame(table(categ))
  table_categ$Freq[table_categ$Freq == 0] = NA
  table_categ = na.omit(table_categ)
  n1 = table_categ[1, 2]
  n2 = table_categ[2, 2]
  g = sqrt(1 / n1 + 1 / n2)
  d = f * g
  l = ci.smd(
    ncp = f,
    n.1 = n1,
    n.2 = n2,
    conf.level = .95
  )
  if (for_table == T) {
    out = paste(
      "t(",
      ro(df, 1),
      ")",
      " = ",
      ro(f),
      ", p = ",
      ro(pvalue, 3),
      ", dbetween = ",
      ro(d, 2),
      " [",
      ro(l$Lower.Conf.Limit.smd, 2),
      ", ",
      ro(l$Upper.Conf.Limit.smd, 2),
      "].",
      sep = ""
    )
  } else {
    out = paste(
      "t(",
      ro(df, 1),
      ")",
      " = ",
      ro(f),
      ", p = ",
      ro(pvalue, 3),
      ", dbetween = ",
      ro(d, 2),
      ", 95% CI [",
      ro(l$Lower.Conf.Limit.smd, 2),
      ", ",
      ro(l$Upper.Conf.Limit.smd, 2),
      "].",
      sep = ""
    )
  }
  v = (n1 + n2) / (n1 * n2) + (d ** 2) / (2 *  (n1 + n2))
  stats <-
    cbind(d, v, out) # so I changed it here so its also outputting the cohens D
}

sed = function (v1, v2) {
  esc::esc_mean_sd(mean(v1), sd(v1), length(v1), mean(v2), sd(v2), length(v2))$se
}


getrtdiffs = function(datf, prefx = '') {
  datf = datf[datf$valid_trial == 1 &&
                datf$type %in% c('probe', 'irrelevant'), ]
  prob = datf$rt[datf$type == 'probe']
  irr = datf$rt[datf$type == 'irrelevant']
  perstim = data.frame(type = character(),
                       rtmean = numeric(),
                       stringsAsFactors = FALSE)
  for (thestim in unique(datf$stim)) {
    stimdat = datf[datf$stim == thestim, ]
    perstim[nrow(perstim) + 1, ] = c(stimdat$type[1], mean(stimdat$rt))
  }
  perstim$rtmean = as.numeric(perstim$rtmean)
  perstim$scaled = scale(perstim$rt)
  return(setNames(c(
    mean(prob) - mean(irr),
    (mean(prob) - mean(irr)) / sd(irr),
    mean(perstim$scaled[perstim$type == "probe"])
  ),
  paste0(
    prefx, c("rt_diff", "rt_dcit", "rt_scaled")
  )))
}

get_sd_info = function(fulprep_dat, preds_list, study, study_num) {
  for (pred_to_get in preds_list) {
    dat_guilt = filter(fulprep_dat,  cond == 1)[[pred_to_get]]
    dat_innocent = filter(fulprep_dat,  cond == 0)[[pred_to_get]]
    #t = t.test(dat_guilt, dat_innocent)$statistic
    n_g = length(dat_guilt)
    n_i = length(dat_innocent)
    sd_g = sd(dat_guilt)
    sd_i = sd(dat_innocent)
    m_g = mean(dat_guilt)
    m_i = mean(dat_innocent)
    row_x = data.frame(
      version = pred_to_get,
      study = study,
      dataset = study_num,
      #t = t,
      sd_g = sd_g,
      sd_i = sd_i,
      n_g = n_g,
      n_i = n_i,
      m_g = m_g,
      m_i = m_i
    )
    if (pred_to_get == preds_list[1]) {
      sd_rows = row_x
    } else {
      sd_rows = rbind(sd_rows, row_x)
    }
  }
  return(sd_rows)
}

# test: get_bf_info(c(1,2,3,4,23,2),c(1,2,13,24,23,2), "stud", 88)

ro = function(value, round_to = 2) {
  return(format(round(value, round_to), nsmall = round_to))
}

sd_pooled = function(var1, var2) {
  n1 = length(var1)
  n2 = length(var2)
  nom = (n1 - 1) * (sd(var1) ** 2) + (n2 - 1) * (sd(var2) ** 2)
  sd_p = sqrt(nom / (n1 + n2 - 2))
  return(sd_p)
}


# this data takes the "raw" data and prepares it for analysis
dat_prep = function(id,
                    gender,
                    age,
                    stim,
                    trial,
                    cond,
                    rt,
                    corr,
                    type,
                    multiple_single,
                    study,
                    studnum) {
  Data_raw <-
    data.frame(
      id = id,
      gender = gender,
      age = age,
      stim = stim,
      trial = trial,
      cond = cond,
      rt = rt,
      corr = corr,
      type = type,
      multiple_single = multiple_single,
      study = study
    )
  Data_raw_test  <<- Data_raw
  # Data_raw  = Data_raw_test

  for (subjectid in enum(unique(Data_raw$id), hush = TRUE, enumerate = FALSE)) {
    subj_itms_base = Data_raw[Data_raw$id == subjectid, ]
    subj_itms_base$too_slow = ifelse(subj_itms_base$rt > 800, 1, 0)
    subj_itms_base$valid_trial = ifelse(
      subj_itms_base$corr == 1 &
        subj_itms_base$too_slow == 0 &
        subj_itms_base$rt >= 150,
      1,
      0
    )
    subj_acc_rates = neatStats::aggr_neat(
      dat = subj_itms_base,
      values = valid_trial,
      method = mean,
      group_by = c("type"),
      filt = (rt >= 150),
      prefix = "acc_rate"
    )


    overall_acc = neatStats::aggr_neat(
      dat = subj_itms_base,
      values = valid_trial,
      method = mean,
      group_by = c("type"),
      prefix = "overall_acc"
    )

    basics = subj_itms_base[1, c('id', 'gender', 'age', 'cond', 'multiple_single', 'study')]

    if (min(overall_acc$aggr_value) > 0.2) {
      subj_rt_mean =
        neatStats::aggr_neat(
          dat = subj_itms_base,
          values = rt,
          method = mean,
          group_by = c("type"),
          filt = (valid_trial == 1),
          prefix = "rt_mean"
        )

      halfnum = (max(subj_itms_base$trial) - min(subj_itms_base$trial)) / 2
      h1 = getrtdiffs(subj_itms_base[subj_itms_base$trial < halfnum,])
      h2 = getrtdiffs(subj_itms_base[subj_itms_base$trial < halfnum,])

      maxblock = nrow(subj_itms_base) %/% 100
      subj_itms_base$type = as.character(subj_itms_base$type)

      for (blocknum in 1:maxblock) {
        if (blocknum == maxblock) {
          df_block = subj_itms_base[(blocknum * 100 - 99):nrow(subj_itms_base),]
        } else {
          df_block = subj_itms_base[(blocknum * 100 - 99):(blocknum * 100),]
        }
        print(blocknum)
      }

      %%%%getrtdiffs(df_block, 'hey')

      rbind_loop(main_cit_merg,
                 basics,
                 subj_acc_rates,
                 subj_rt_mean,
                 subj_rt_mean1,
                 subj_rt_mean2,
                 overall_acc)
    } else {
      rbind_loop(main_cit_merg,
                 basics,
                 subj_acc_rates,
                 overall_acc)
    }

  }
  main_cit_merg$dataset = studnum
  return(main_cit_merg)
}

auc_ci = function(auc_obj, which_ci, ci = 0.95) {
  pROC::ci.auc(auc_obj, conf.level = ci)[which_ci]
}

effectsize_data = function(id,
                           pred_all,
                           pred_1,
                           pred_2,
                           cond,
                           multiple_single,
                           study,
                           sd_sim) {
  Data_Real <-
    tibble (
      id = id,
      pred_all = pred_all,
      pred_1 = pred_1,
      pred_2 = pred_2,
      cond = cond,
      multiple_single = multiple_single,
      study = study
    )

  testData_Real <<- Data_Real
  # Data_Real = testData_Real

  multiple_single <-
    ifelse(Data_Real$multiple_single[1] == 1, "multiple", "single")
  multiple_single <-
    ifelse(Data_Real$multiple_single[1] == 2,
           "inducer",
           multiple_single)
  study <- Data_Real$study[1]

  pred_all_d <-
    cohen_d_between(Data_Real$pred_all, Data_Real$cond)

  pred_1_d <-
    cohen_d_between(Data_Real$pred_1, Data_Real$cond)
  pred_2_d <-
    cohen_d_between(Data_Real$pred_2, Data_Real$cond)


  pred_all_sed = sed(Data_Real$pred_all[Data_Real$cond == 1],
                     Data_Real$pred_all[Data_Real$cond == 0])
  pred_1_sed = sed(Data_Real$pred_1[Data_Real$cond == 1],
                   Data_Real$pred_1[Data_Real$cond == 0])
  pred_2_sed = sed(Data_Real$pred_2[Data_Real$cond == 1],
                   Data_Real$pred_2[Data_Real$cond == 0])


  pred_all_auc = t_neat(
    Data_Real$pred_all[Data_Real$cond == 1],
    Data_Real$pred_all[Data_Real$cond == 0],
    auc_added = TRUE,
    bf_added = FALSE, hush = TRUE
  )

  pred_1_auc = t_neat(Data_Real$pred_1[Data_Real$cond == 1],
                      Data_Real$pred_1[Data_Real$cond == 0],
                      auc_added = TRUE, hush = TRUE,
                      bf_added = FALSE)

  pred_2_auc = t_neat(Data_Real$pred_2[Data_Real$cond == 1],
                      Data_Real$pred_2[Data_Real$cond == 0],
                      auc_added = TRUE, hush = TRUE,
                      bf_added = FALSE)
  #

  the_versions = c("pred_all", "pred_1", "pred_2")

  output <-
    tibble(
      cohens_d = c(-as.numeric(pred_all_d[1]),
                   -as.numeric(pred_1_d[1]),
                   -as.numeric(pred_2_d[1])),
      variance_d = c(as.numeric(pred_all_d[2]),
                     as.numeric(pred_1_d[2]),
                     as.numeric(pred_2_d[2])),
      version = the_versions,
      multiple_single = rep(multiple_single, length(the_versions)),
      study = rep(study, length(the_versions)),
      sed = c(pred_all_sed,
              pred_1_sed,
              pred_2_sed),
      aucs = c(
        as.numeric(pred_all_auc$stats['auc']),
        as.numeric(pred_1_auc$stats['auc']),
        as.numeric(pred_2_auc$stats['auc'])
      ),
      auc_lower = c(
        auc_ci(pred_all_auc$roc_obj, 1),
        auc_ci(pred_1_auc$roc_obj, 1),
        auc_ci(pred_2_auc$roc_obj, 1)
      ),
      auc_upper = c(
        auc_ci(pred_all_auc$roc_obj, 3),
        auc_ci(pred_1_auc$roc_obj, 3),
        auc_ci(pred_2_auc$roc_obj, 3)
      ),
      accuracies = c(
        as.numeric(pred_all_auc$stats['youden'] + 1) / 2,
        as.numeric(pred_1_auc$stats['youden'] + 1) / 2,
        as.numeric(pred_2_auc$stats['youden'] + 1) / 2
      ),
      thresholds = c(
        as.numeric(pred_all_auc$best_thresholds['threshold']),
        as.numeric(pred_1_auc$best_thresholds['threshold']),
        as.numeric(pred_2_auc$best_thresholds['threshold'])
      ),
      TNs = c(
        as.numeric(pred_all_auc$best_thresholds['specificity']),
        as.numeric(pred_1_auc$best_thresholds['specificity']),
        as.numeric(pred_2_auc$best_thresholds['specificity'])
      ),
      TPs = c(
        as.numeric(pred_all_auc$best_thresholds['sensitivity']),
        as.numeric(pred_1_auc$best_thresholds['sensitivity']),
        as.numeric(pred_2_auc$best_thresholds['sensitivity'])
      )
    )
  return(output)
}
