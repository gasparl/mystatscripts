library(neatStats)

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

# props_neat(900*0.76, 900*0.70, 900, 900)

utils::write.table(
  pred_final,
  "cit_predictors_meta.txt",
  sep = "\t",
  quote = FALSE,
  row.names = FALSE
)


pred_final$rt_mean_diff_scaled = pred_final$rt_mean_diff / (pred_final$rt_mean_target - pred_final$rt_mean_irrelevant)
rt_pred_alt = t_neat(pred_final$rt_mean_diff_scaled[pred_final$outcome == 1],
       pred_final$rt_mean_diff_scaled[pred_final$outcome == 0],
       auc_added = TRUE)

pred_final$acc_rate_diff_scaled = pred_final$acc_rate_diff / (pred_final$acc_rate_target - pred_final$acc_rate_irrelevant)
acc_pred_alt = t_neat(pred_final$acc_rate_diff_scaled[pred_final$outcome == 0],
                       pred_final$acc_rate_diff_scaled[pred_final$outcome == 1],
                       auc_added = TRUE)

roc_neat(rt_pred_base$roc_obj, rt_pred_alt$roc_obj, pair = TRUE)
roc_neat(acc_pred_base$roc_obj, acc_pred_alt$roc_obj, pair = TRUE)
