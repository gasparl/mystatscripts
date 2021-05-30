# libs ----
library("neatStats")
library("ggplot2")
#
# 20,000 samples per second

setwd(path_neat(""))
filenames = list.files(pattern = "^disptime.*.csv$")
#filenames = list.files(pattern = "^disptime_Windows_Microsoft Edge_white_2021_0530_1404*.csv$")

secres = 20000

dat_merged = data.frame()
for (fname in filenames) {
  # fname = 'disptime_Windows_Chrome_black_20210527125457_pilot1b.csv'
  # fname = filenames[1]
  cat(fname, fill = TRUE)
  raw_sampls = read.table(
    fname,
    sep = ",",
    header = TRUE,
    fill = TRUE,
    quote = "\"",
    stringsAsFactors = FALSE
  )
  raw_sampls = raw_sampls[secres:nrow(raw_sampls), ]
  rt_data = read.table(
    sub('.csv', '.txt', fname, fixed = TRUE),
    sep = "\t",
    header = TRUE,
    fill = TRUE,
    quote = "\"",
    stringsAsFactors = FALSE
  )

  dems_row = rt_data[startsWith(as.character(rt_data$datetime), 'client'),]
  dems_heads = strsplit(dems_row[[2]], "/")[[1]]
  dems_dat = strsplit(dems_row[[3]], "/")[[1]]
  dems = do.call(rbind.data.frame, list(dems_dat))
  colnames(dems) = dems_heads

  # midpoint = (min(datapoints) + max(datapoints))/2
  # overmid = which(datapoints > midpoint)
  # datapoints = datapoints[min(overmid):max(overmid)]

  sampls = data.frame(
    sample = seq.int(raw_sampls$V) / secres,
    values = raw_sampls$V,
    triggers = raw_sampls$logic,
    version = fname
  )
  if (dems$bg == 'white') {
    sampls$values = -sampls$values
    cutoff_upp = (max(sampls$values) + min(sampls$values)) / 3
    cutoff_low = (max(sampls$values) + min(sampls$values)) / 2
  } else {
    cutoff_upp = (max(sampls$values) + min(sampls$values)) / 2
    cutoff_low = (max(sampls$values) + min(sampls$values)) / 3
  }

  trial_data = data.frame()
  last_start = secres
  trialnum = 0
  for (index in secres:(length(sampls$triggers) - secres)) {
    if (trialnum > 3000) {
      break
    }
    if (trialnum > 1 &&
        index == (last_start + secres * 1.5)) {
      trialnum = trialnum + 1
      message("skipped TRIGGER at ", index, ", trial ", trialnum)
    }
    # detect trigger start from 900 ms from last start
    if (index > (last_start + secres * 0.9) &&
        sampls$triggers[index] == 1) {

      # make extra checks for trigger start
      if (mean(sampls$triggers[(index - 10):(index - 1)]) < 0.5 &&
          mean(sampls$triggers[(index):(index + secres * 0.1)]) > 0.9 &&
          mean(sampls$triggers[(index + secres * 0.2):(index + secres * 0.4)]) < 0.1) {

        # detect trigger end between 80 and 180 ms after trigger start
        for (endindx in (index + secres * 0.08):(index + secres * 0.18)) {
          if (sampls$triggers[endindx] == 0 &&
              mean(sampls$triggers[(endindx):(endindx + secres * 0.01)]) < 0.1) {
            # if found trigger, try get display info
            disp_start = NA
            disp_end = NA
            # look for disp start between 0 and 200 ms after trigger
            for (start_i in (index):(index + secres * 0.2)) {
              if (sampls$values[start_i] > cutoff_low &&
                  mean(sampls$values[(start_i):(start_i + secres * 0.005)]) > cutoff_low &&
                  mean(sampls$values[(start_i + secres * 0.005):(start_i + secres * 0.015)]) > cutoff_upp) {
                if (start_i < (index + secres * 0.005)) {
                  if (is.na(tail(trial_data$disp_start, 3)[1]) &&
                      is.na(tail(trial_data$disp_start, 2)[1])) {
                    message("END at ", index, ", trial ", trialnum)
                    trialnum = 9999
                  } else {
                    stop('suspiciously fast change... ')
                  }
                }
                disp_start = start_i
                break
              }
            }
            if (!is.na(disp_start)) {
              for (end_i in (disp_start + secres * 0.015):(index + secres * 0.6)) {
                if (sampls$values[end_i] < cutoff_upp &&
                    mean(sampls$values[(end_i):(end_i + secres * 0.005)]) < cutoff_upp &&
                    mean(sampls$values[(end_i + secres * 0.005):(end_i + secres * 0.015)]) < cutoff_low) {
                  disp_end = end_i
                  break
                }
              }
            }

            # save stuff
            trial_data = rbind(
              trial_data,
              data.frame(
                trial_number = trialnum,
                keydown = index,
                keyup = endindx,
                disp_start = disp_start,
                disp_end = disp_end
              )
            )

            # since trigger was found, move on to next trial
            last_start = index
            trialnum = trialnum + 1
            break
          }
        }
      }
    }
  }
  trial_data$pressdur = (trial_data$keyup - trial_data$keydown) / secres
  # sanity checks
  if (abs(trial_data$pressdur[1] - 0.16) < 0.001) {
    message('correct start: ', trial_data$pressdur[1])
  } else {
    stop('start duration problem (1): ', trial_data$pressdur[1])
  }
  for (trial in 2:nrow(trial_data)) {
    if (trial %% 10 == 1) {
      if (!abs(trial_data$pressdur[trial] - 0.13) < 0.001) {
        stop('start duration problem (2): ',
             trial,
             ', ',
             trial_data$pressdur[trial])
      }
    } else {
      if (!abs(trial_data$pressdur[trial] - 0.1) < 0.001) {
        stop('start duration problem (3): ',
             trial,
             ', ',
             trial_data$pressdur[trial])
      }
    }
    iti = (trial_data$keydown[trial] - trial_data$keydown[trial - 1]) / secres
    if (iti < 0.9 | iti > 1.15) {
      stop('interval problem: ',
           trial,
           ', ',
           iti)
    }
  }

  cond_data = merge(rt_data, trial_data, by = 'trial_number')
  if (nrow(cond_data) < nrow(rt_data) - 1) {
    stop("missing trials: ", nrow(cond_data), " vs ", nrow(rt_data))
  }

  cond_data$background = dems$bg[1]
  cond_data$OS = dems$os[1]
  cond_data$Browser = dems$browser[1]
  cond_data$file = fname
  dat_merged = rbind(dat_merged, cond_data)
}

full_data = dat_merged

###

# full_data = full_data[full_data$file == fname,]

full_data$js_start = full_data$js_start_stamp
full_data$js_end = full_data$js_end_stamp

full_data$d1_ext = (full_data$disp_start - full_data$keydown) / secres * 1000
full_data$d1_js = full_data$js_start - full_data$js_input
full_data$d1_diff = full_data$d1_js - full_data$d1_ext

full_data$d2_ext = (full_data$disp_end - full_data$disp_start) / secres * 1000
full_data$d2_ext[is.na(full_data$d2_ext)] = 0
full_data$d2_js = full_data$js_end - full_data$js_start

full_data$d2_diff = full_data$d2_js - full_data$d2_ext
if (mean(abs(full_data$d2_diff)) > 20) {
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
colrs = viridis::viridis(3, end = 0.85)

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

ggplot(pdata, aes(x = d1_ext)) + ggtitle(n_dict['d1_ext']) + xlab(n_dict['dur']) +
  geom_histogram(aes(color = NULL, fill = Browser), bins = 100)  +
  theme_bw() + scale_fill_manual(values = colrs) + facet_wrap(vars(timer, Browser))

ggplot(pdata, aes(x = d1_js)) + ggtitle(n_dict['d1_js']) + xlab(n_dict['dur']) +
  geom_histogram(aes(color = NULL, fill = Browser), bins = 100)  +
  theme_bw() + scale_fill_manual(values = colrs) + facet_wrap(vars(timer, Browser))

ggplot(pdata, aes(x = d1_diff)) + ggtitle(n_dict['d1_diff']) + xlab(n_dict['diff']) +
  geom_histogram(aes(color = NULL, fill = Browser), bins = 100)  +
  theme_bw() + scale_fill_manual(values = colrs) + facet_wrap(vars(timer, Browser))

ggplot(pdata, aes(x = d2_diff)) + ggtitle(n_dict['d2_diff']) + xlab(n_dict['diff']) +
  geom_histogram(aes(color = NULL, fill = Browser), bins = 100)  +
  theme_bw() + scale_fill_manual(values = colrs) + facet_wrap(vars(timer, Browser))

ggplot(pdata, aes(x = d3_diff)) + ggtitle(n_dict['d3_diff']) + xlab(n_dict['dur']) +
  geom_histogram(aes(color = NULL, fill = Browser), bins = 100)  +
  theme_bw() + scale_fill_manual(values = colrs) + facet_wrap(vars(timer, Browser))

dat_comp = full_data[full_data$timer %in% c('none', 'rAF_loop'),]
dat_comp = full_data[full_data$timer %in% c('rAF_loop', 'rPAF_loop'),]
dat_comp = full_data[full_data$timer %in% c('rAF_loop', 'rPAF_loop'),]
var_tests(dat_comp$d2_diff, dat_comp$timer)

ggplot(pdata, aes(x = d2_ext)) + ggtitle(n_dict['d2_ext']) + xlab(n_dict['dur']) +
  geom_histogram(aes(color = NULL, fill = Browser), bins = 100)  +
  theme_bw() + scale_fill_manual(values = colrs) + facet_wrap(vars(timer, Browser))

# check issues
dcheck = full_data[full_data$timer == 'rAF_loop' & full_data$Browser == 'Firefox',]

dcheck$correct = ifelse(abs(dcheck$d2_diff) < 5, 'Correct', 'Short')
ggplot(dcheck, aes(x = duration)) + xlab('Expected durations') +
  geom_histogram(aes(color = NULL), bins = 100)  +
  theme_bw() + facet_wrap(vars(correct))


## visual etc checks

# unique(sampls$triggers)
# ggpubr::gghistogram((c(trial_data$keydown[-1]) -
#                      trial_data$keydown[-length(trial_data$keydown)]) / secres)

#

# d_small = sampls[sampls$sample > 13 & sampls$sample < 35, ]
# t_small = trial_data[1:15]
# # d_small = sampls[seq(1, nrow(sampls), 200), ]
# # d_small = sampls[(17372722-secres*3):(17372722+secres*4),]
# p = ggplot(data = d_small, aes(x = sample, y = values, group = 1)) +
#   geom_line(color = 'blue') +
#   geom_line(aes(y = triggers), color = 'red') +
#   geom_vline(xintercept = t_small$keydown / secres, color = 'green') +
#   geom_vline(xintercept = t_small$keyup / secres, color = 'grey') +
#   geom_vline(xintercept = t_small$disp_start / secres, color = 'darkgreen') +
#   geom_vline(xintercept = t_small$disp_end / secres, color = 'black')
# p
# plotly::ggplotly(p)

# colrs = viridis::viridis(3, end = .85)
# p = ggplot(data = allsampls, aes(x = sample, y = values, group = 1)) +
#   geom_line() + scale_color_manual(values = colrs) + facet_wrap(vars(version))
