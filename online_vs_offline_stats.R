library('ggplot2')
library('data.table')
library('trend')
library('tsbox')
library('ppcor')

oo_data = readRDS(neatStats::path_neat('online_vs_offline_data.rds'))

names(oo_data)[names(oo_data) == "year"] = "time"

# replace outliers with ceiling
oo_data_total$total = oo_data_total$offline + oo_data_total$online
oo_data_total$ratio = oo_data_ratio$online / (oo_data_ratio$online + oo_data_ratio$offline)
oo_data$total[oo_data$total > 1000] = 1000

# total samples
oo_data_total = oo_data
oo_data_total$value = oo_data_total$total
ts_total = tsbox::ts_ts(oo_data_total[, c('time', 'journal', 'value')])
ts_total[is.na(ts_total)] = 100
mult.mk.test(ts_total)


# online ratio
oo_data_ratio = oo_data
oo_data_ratio$value = oo_data_ratio$ratio
ts_ratio = tsbox::ts_ts(oo_data_ratio[, c('time', 'journal', 'value')])
ts_ratio[is.na(ts_ratio)] = 100
mult.mk.test(ts_ratio)

# citation and authorship

pcor.test(oo_data$total, oo_data$citations, oo_data$time)
pcor.test(oo_data$ratio, oo_data$citations, oo_data$time)

pcor.test(oo_data$total, oo_data$authors, oo_data$time)
pcor.test(oo_data$ratio, oo_data$citations, oo_data$time)

