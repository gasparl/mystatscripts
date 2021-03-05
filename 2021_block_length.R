library('neatStats')

# Set working directory
setwd(path_neat())

main_preds = utils::read.table(
  "cit_predictors_meta.txt",
  sep = "\t",
  header = TRUE,
  fill = TRUE,
  quote = "\"",
  stringsAsFactors = FALSE
)
