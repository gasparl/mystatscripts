library('neatStats')
library('ggplot2')

# Set working directory
setwd(path_neat())

load("cit_meta_data_trial_level.Rda")

main_preds = utils::read.table(
  "cit_predictors_meta.txt",
  sep = "\t",
  header = TRUE,
  fill = TRUE,
  quote = "\"",
  stringsAsFactors = FALSE
)

trialpred_data = cit_meta_data_trial_level[cit_meta_data_trial_level$subject_id %in% main_preds$id, ]
trialpred_data = trialpred_data[trialpred_data$rt <= 800 &
                                  trialpred_data$rt > 150 &
                                  trialpred_data$type %in% c('probe', 'irrelevant', 'target'), ]

trialpred = trialpred_data[order(
  trialpred_data$subject_id,
  trialpred_data$block,
  trialpred_data$trial
),]

trialpred$trial_number = sequence(rle(trialpred$subject_id)$lengths)

trialpred = trialpred[, c(
  "study",
  "condition",
  "multiple_single",
  "subject_id",
  "dataset",
  "type",
  "correct",
  "rt",
  "trial_number"
)]
colnames(trialpred) = c(
  "study",
  "outcome",
  "cit_version",
  "subject_id",
  "dataset",
  "item_type",
  "correct",
  "rt",
  "trial_number"
)

# myplot = ggplot(data = trialpred, aes(x = trial_number, y = rt)) +  geom_smooth(aes(color = item_type)) + facet_wrap(vars(outcome))
# trials_guilty = trialpred[trialpred$outcome == 1,]
# myplot = ggplot(data = trials_guilty, aes(x = trial_number, y = rt)) +  geom_smooth(aes(color = item_type)) + facet_wrap(vars(dataset))
# myplot

utils::write.table(
  trialpred,
  "cit_trialpreds_meta.txt",
  sep = "\t",
  quote = FALSE,
  row.names = FALSE
)

library("openxlsx")
write.xlsx(trialpred, 'cit_trialpreds_meta.xlsx', sheetName = 'Sheet1')

