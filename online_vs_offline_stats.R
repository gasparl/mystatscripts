library('ggplot2')
library('data.table')
library('trend')
library('tsbox')
library('ppcor')

oo_data = readRDS(neatStats::path_neat('online_vs_offline_data.rds'))

names(oo_data)[names(oo_data) == "year"] = "time"

# replace outliers with ceiling
oo_data$online[oo_data$online > 1000] = 1000
oo_data$offline[oo_data$offline > 1000] = 1000

oo_data$total = oo_data$offline + oo_data$online
oo_data$ratio = oo_data$online / oo_data$total

jnl_data = oo_data[, .(
    total = mean(total),
    online = mean(online),
    offline = mean(offline),
    ratio = mean(ratio)
), by = c('journal', 'time')]

# jnl_data = jnl_data[jnl_data$journal!='Psychonomic Bulletin and Review']
# jnl_data2 = jnl_data
# jnl_data2$time = jnl_data2$time+1
# jnl_data = rbind(jnl_data, jnl_data2)

# total samples
jnl_data_total = jnl_data
jnl_data_total$value = jnl_data_total$total
ts_total = tsbox::ts_ts(jnl_data_total[, c('time', 'journal', 'value')])
ts_total[is.na(ts_total)] = 100
mult.mk.test(ts_total, alternative = 'greater')

ggplot(jnl_data_total,aes(time, value)) +
    stat_summary(geom = "line", fun.y = mean) +
    stat_summary(geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.3)

ggplot(jnl_data_offline,aes(time, value)) +
    stat_summary(geom = "line", fun.y = mean) +
    stat_summary(geom = "ribbon", fun.data = mean_cl_normal, alpha = 0.3)


# online ratio
jnl_data_ratio = jnl_data
jnl_data_ratio$value = jnl_data_ratio$ratio
ts_ratio = tsbox::ts_ts(jnl_data_ratio[, c('time', 'journal', 'value')])
ts_ratio[is.na(ts_ratio)] = 100
mult.mk.test(ts_ratio, alternative = 'greater')

# offline alone
jnl_data_offline = jnl_data
jnl_data_offline$value = jnl_data_offline$offline
ts_offline = tsbox::ts_ts(jnl_data_offline[, c('time', 'journal', 'value')])
ts_offline[is.na(ts_offline)] = 100
mult.mk.test(ts_offline, alternative = 'greater')


# citation and authorship

oo_data$citations= as.numeric(oo_data$citations)
oo_data$authors=as.numeric(oo_data$authors)
pcor.test(oo_data$total, oo_data$citations, oo_data$time)
pcor.test(oo_data$ratio, oo_data$citations, oo_data$time)

pcor.test(oo_data$total, oo_data$authors, oo_data$time, method = "kendall")
pcor.test(oo_data$ratio, oo_data$authors, oo_data$time, method = "kendall")

pcor.test(oo_data$total, oo_data$ratio, oo_data$time)
pcor.test(oo_data$authors, oo_data$citations, oo_data$time, method = "kendall")


