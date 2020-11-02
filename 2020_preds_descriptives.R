library("neatStats")
rng = function(num) {
  res = c(mean = mean(num), 
          neatStats::mean_ci(num, distance_only = FALSE))
  neatStats::mean_ci(num, distance_only = FALSE)
  res['min'] = min(res)
  res['max'] = max(res)
  return(round(res, digits = 2))
}

# Set working directory
setwd(path_neat())

pred_data = readRDS("all_predictors_meta.rds")

pred_data$gender[pred_data$gender == 9] = NA
pred_data$age[pred_data$age < 15 | pred_data$age > 95] = NA

dems_neat(pred_data, group_by = 'cond')

length(unique(pred_data$id))
length(unique(pred_data$study))
length(unique(pred_data$dataset))


pred_filt = excl_neat(pred_data,
                      filt = (
                        acc_rate_target > 0.5 &
                          overall_acc_allmain > 0.75 &
                          (is.na(overall_acc_itarget) |
                             overall_acc_itarget > 0.5) &
                          (is.na(overall_acc_nontarget) | overall_acc_nontarget > 0.5)
                      ), excluded = TRUE)

pred_excl = pred_filt$excluded
pred_final = pred_filt$filtered

pred_final$rt_mean_ti_diff = pred_final$rt_mean_target - pred_final$rt_mean_irrelevant
pred_final$acc_rate_ti_diff = pred_final$acc_rate_target - pred_final$acc_rate_irrelevant

dems_neat(pred_final, group_by = 'cond')
names(pred_final)[names(pred_final) == 'cond'] <- 'outcome'

rt_pred_base = t_neat(pred_final$rt_mean_diff[pred_final$outcome == 1],
                      pred_final$rt_mean_diff[pred_final$outcome == 0],
                      auc_added = TRUE)
acc_pred_base = t_neat(pred_final$acc_rate_diff[pred_final$outcome == 0],
                       pred_final$acc_rate_diff[pred_final$outcome == 1],
                       auc_added = TRUE)

pred_final = pred_final[order(nchar(pred_final$id), pred_final$id),]

###

pred_guilt = pred_final[pred_final$outcome == 1,]
pred_control = pred_final[pred_final$outcome == 0,]

mean(pred_guilt$rt_mean_probe)
mean(pred_guilt$rt_mean_irrelevant)
corr_neat(pred_guilt$rt_mean_probe, pred_guilt$rt_mean_irrelevant)

means_prob_g = c()
means_prob_i = c()
sds_prob_g = c()
sds_prob_i = c()
means_irr_g = c()
means_irr_i = c()
sds_irr_g = c()
sds_irr_i = c()
corrs_g = c()
corrs_i = c()
for (datnum in sort(unique(pred_final$dataset))) {
  preds = pred_final[pred_final$dataset == datnum,]
  print(preds$study[1])
  pred_guilt = preds[preds$outcome == 1,]
  pred_control = preds[preds$outcome == 0,]
  means_prob_g = c(means_prob_g, mean(pred_guilt$rt_mean_probe))
  means_prob_i = c(means_prob_i, mean(pred_control$rt_mean_probe))
  sds_prob_g = c(sds_prob_g, sd(pred_guilt$rt_mean_probe))
  sds_prob_i = c(sds_prob_i, sd(pred_control$rt_mean_probe))
  
  means_irr_g = c(means_irr_g, mean(pred_guilt$rt_mean_irrelevant))
  means_irr_i = c(means_irr_i, mean(pred_control$rt_mean_irrelevant))
  sds_irr_g = c(sds_irr_g, sd(pred_guilt$rt_mean_irrelevant))
  sds_irr_i = c(sds_irr_i, sd(pred_control$rt_mean_irrelevant))
  
  mycor_g = corr_neat(pred_guilt$rt_mean_probe, pred_guilt$rt_mean_irrelevant)
  corrs_g = c(corrs_g, mycor_g['r'])
  mycor_i = corr_neat(pred_control$rt_mean_probe, pred_control$rt_mean_irrelevant)
  corrs_i = c(corrs_i, mycor_i['r'])
}

rng(means_prob_g)
rng(means_irr_g)
rng(sds_prob_g)
rng(sds_irr_g)
rng(corrs_g)

rng(means_prob_i)
rng(means_irr_i)
rng(sds_prob_i)
rng(sds_irr_i)
rng(corrs_i)

