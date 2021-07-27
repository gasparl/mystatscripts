# libs ----
library("neatStats")
library("ggplot2")
#
# 20,000 samples per second

secres = 20000
msecres = secres / 1000
colrs = viridis::viridis(4, end = 0.85)

setwd(path_neat(""))

full_data = readRDS("2021_disp_time_aggr.rds")
# full_data = full_data[full_data$file == "disptime_Windows_Firefox_white_2021_0530_1343.csv",]

full_data = full_data[full_data$study == 'plain',]
#full_data = full_data[full_data$study == 'image',]

###

# full_data = full_data[full_data$file == fname,]

full_data$js_start = full_data$js_start_stamp
full_data$js_end = full_data$js_end_stamp

full_data$d1_ext = (full_data$disp_start - full_data$keydown) / msecres
full_data$d1_js = full_data$js_start - full_data$js_input
full_data$d1_diff = full_data$d1_js - full_data$d1_ext

full_data$d2_ext = (full_data$disp_end - full_data$disp_start) / msecres
full_data$d2_ext[is.na(full_data$d2_ext)] = 0
full_data$d2_js = full_data$js_end - full_data$js_start

full_data$d2_diff = full_data$d2_js - full_data$d2_ext
if (mean(abs(full_data$d2_diff)) > 25) {
  stop('inconsistent timings: ', mean(abs(full_data$d2_diff)))
}

full_data$d3_diff = full_data$duration - full_data$d2_ext

# Best is with:
# smallest full_data$d1_ext
# least variability in full_data$d1_diff and full_data$d2_diff
# full_data$d2_diff closest to zero

pdata = full_data
# pdata = full_data[full_data$duration == 50,]
# pdata = full_data[full_data$duration == 16,]
binw = 2
n_dict = c(
  d1_ext = 'Real response time (from keydown to display start)',
  d1_js = 'JS timed response time (from keydown to display start)',
  d1_diff = 'Difference in response time (from keydown to display): JS minus real',
  d2_ext = 'Real duration (from display start to display end)',
  d2_js = 'JS timed duration (from display start to display end)',
  d2_diff = 'Difference in duration (from display to display end): JS minus real',
  d3_diff = 'Expected display duration minus real display duration',
  dur = 'Duration (ms)',
  diff = 'Difference (ms)'
)

#pdata = pdata[pdata$type == 'img_large', ]

ggplot(pdata, aes(x = d1_ext)) + ggtitle(n_dict['d1_ext']) + xlab(n_dict['dur']) +
  geom_histogram(aes(color = NULL, fill = Browser), binwidth = binw)  +
  theme_bw() + scale_fill_manual(values = colrs) + facet_wrap(vars(method, Browser)) +
  theme(legend.position = "top")

ggplot(pdata, aes(x = d1_js)) + ggtitle(n_dict['d1_js']) + xlab(n_dict['dur']) +
  geom_histogram(aes(color = NULL, fill = Browser), binwidth = binw)  +
  theme_bw() + scale_fill_manual(values = colrs) + facet_wrap(vars(method, Browser)) +
  theme(legend.position = "top")

ggplot(pdata, aes(x = d1_diff)) + ggtitle(n_dict['d1_diff']) + xlab(n_dict['diff']) +
  geom_histogram(aes(color = NULL, fill = Browser), binwidth = binw)  +
  theme_bw() + scale_fill_manual(values = colrs) + facet_wrap(vars(method, Browser)) +
  theme(legend.position = "top")

# [full_data$d2_ext != 0,]
# [pdata$d2_diff < 300,]
ggplot(pdata, aes(x = d2_diff)) + ggtitle(n_dict['d2_diff']) + xlab(n_dict['diff']) +
  geom_histogram(aes(color = NULL, fill = Browser), binwidth = binw)  +
  theme_bw() + scale_fill_manual(values = colrs) + facet_wrap(vars(method, Browser)) +
  theme(legend.position = "top")

# [pdata$d3_diff < 300,]
ggplot(pdata, aes(x = d3_diff)) + ggtitle(n_dict['d3_diff']) + xlab(n_dict['dur']) +
  geom_histogram(aes(color = NULL, fill = Browser), binwidth = binw)  +
  theme_bw() + scale_fill_manual(values = colrs) + facet_wrap(vars(method, Browser)) +
  theme(legend.position = "top")


dat_comp = full_data[full_data$method %in% c('none', 'rAF_loop'),]
dat_comp = full_data[full_data$method %in% c('rAF_loop', 'rPAF_loop'),]
dat_comp = full_data[full_data$method %in% c('rAF_loop', 'rPAF_loop'),]
var_tests(dat_comp$d2_diff, dat_comp$method)


ggplot(pdata, aes(x = d2_ext)) + ggtitle(n_dict['d2_ext']) + xlab(n_dict['dur']) +
  geom_histogram(aes(color = NULL, fill = Browser), binwidth = binw)  +
  theme_bw() + scale_fill_manual(values = colrs) + facet_wrap(vars(method, Browser)) +
  theme(legend.position = "top")

# check issues
dcheck = full_data[full_data$method == 'rAF_loop' & full_data$Browser == 'Firefox',]
# dcheck = full_data[full_data$method == 'rPAF_loop' & full_data$Browser == 'Chrome',]

dcheck$correct = ifelse(abs(dcheck$d2_diff) < 5, 'Correct', 'Long')
dcheck$correct = ifelse(abs(dcheck$d3_diff) < 5, 'Correct', 'Long')
ggplot(dcheck, aes(x = duration)) + xlab('Expected durations') +
  geom_histogram(aes(color = NULL), bins = 30)  +
  theme_bw() + facet_wrap(vars(correct))


