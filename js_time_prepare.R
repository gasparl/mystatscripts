# libs ----
library("neatStats")
library("ggplot2")
#
# 20,000 samples per second

setwd(path_neat("study1"))
filenames = list.files(pattern = "^disptime.*.csv$")
print(filenames)
#filenames = list.files(pattern = "^disptime_psy.*.csv$")
#filenames = list.files(pattern = "^disptime_plain_Windows_Chrome_white_2021_0929_1218.*.csv$")

secres = 20000

dat_merged = data.frame()
for (fname in filenames) {
  # fname = 'disptime_image_Windows_Opera_white_2021_0714_1531.csv'
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
  raw_sampls = raw_sampls[secres:nrow(raw_sampls),]
  rt_data = read.table(
    sub('.csv', '.txt', fname, fixed = TRUE),
    sep = "\t",
    header = TRUE,
    fill = TRUE,
    quote = "\"",
    stringsAsFactors = FALSE
  )

  dems_row = rt_data[startsWith(as.character(rt_data$datetime), 'client'), ]
  dems_heads = strsplit(dems_row[[2]], "/")[[1]]
  dems_dat = strsplit(dems_row[[3]], "/")[[1]]
  dems = do.call(rbind.data.frame, list(dems_dat))
  colnames(dems) = dems_heads

  for (trial in 2:(nrow(rt_data) - 1)) {
    iti = (rt_data$js_input[trial] - rt_data$js_input[trial - 1])
    if (iti < 800 | iti > 1300) {
      message('skipped keys: ',
              trial,
              ', ',
              iti)
    }
  }

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
              # look for disp END between 10 and 700 ms after disp_start
              for (end_i in (disp_start + secres * 0.010):(index + secres * 0.7)) {
                if (sampls$values[end_i] < cutoff_upp &&
                    mean(sampls$values[(end_i):(end_i + secres * 0.005)]) < cutoff_upp &&
                    mean(sampls$values[(end_i + secres * 0.005):(end_i + secres * 0.015)]) < cutoff_low) {
                  disp_end = end_i
                  break
                }
              }
            }

            if ((is.na(disp_start)) & grepl('psychopy', fname) &
                is.na(disp_end) &
                  trialnum > 1 & trialnum < nrow(rt_data)) {
              message('missed key at trial ', trialnum)
              trialnum = trialnum - 1
            } else {

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
            }

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
        message('start duration problem (2): ',
                trial,
                ', ',
                trial_data$pressdur[trial])
      }
    } else {
      if (!abs(trial_data$pressdur[trial] - 0.1) < 0.001) {
        message('start duration problem (3): ',
                trial,
                ', ',
                trial_data$pressdur[trial])
      }
    }
    iti = (trial_data$keydown[trial] - trial_data$keydown[trial - 1]) / secres
    if (iti < 0.9 | iti > 1.15) {
      message('interval problem: ',
              trial,
              ', ',
              iti)
    }
  }

  cond_data = merge(rt_data, trial_data, by = 'trial_number')
  if (nrow(cond_data) < nrow(rt_data) - 1) {
    stop("missing trials: ", nrow(cond_data), " vs ", nrow(rt_data))
  }

  cond_data$study = dems$study[1]
  cond_data$background = dems$bg[1]
  cond_data$OS = dems$os[1]
  cond_data$Browser = dems$browser[1]
  cond_data$file = fname
  dat_merged = rbind(dat_merged, cond_data)
}

full_data_out = dat_merged
full_data_out$trial_number = as.numeric(as.character(full_data_out$trial_number))
# saveRDS(full_data_out, path_neat("2021_disp_time_aggr.rds"))

# saveRDS(full_data_out, path_neat("2021_disp_time_aggr_study1.rds"))

# full_data_out = full_data_out[full_data_out$trial_number != 71 | full_data_out$datetime != "2021_0919_0945", ]
# saveRDS(full_data_out, path_neat("2021_disp_time_aggr_python.rds"))

sum(is.na(full_data_out$disp_end))
# [1] 228

# str(full_data_out)

#full_data_out$dispdiff =  (full_data_out$disp_end - full_data_out$disp_start) / secres * 1000
#full_data_out$dispdiff_x = full_data_out$dispdiff - full_data_out$duration

# saveRDS(full_data_out, path_neat("2021_disp_time_aggr_python.rds"))

###

# full_data = full_data[full_data$file == fname,]

# Best is with:
# smallest full_data$d1_ext
# least variability in full_data$d1_diff and full_data$d2_diff
# full_data$d2_diff closest to zero

## visual etc checks

# unique(sampls$triggers)
# ggpubr::gghistogram((c(trial_data$keydown[-1]) -
#                      trial_data$keydown[-length(trial_data$keydown)]) / secres)

#

d_small = sampls[sampls$sample > 13 & sampls$sample < 35,]
# t_small = trial_data[1:15, ]
# d_small = sampls[seq(1, nrow(sampls), 200), ]
d_small = sampls[(3077561 - secres * 3):(3077561 + secres * 4), ]
ggplot(data = d_small, aes(x = sample, y = values, group = 1)) +
  geom_line(color = 'blue') +
  geom_line(aes(y = triggers), color = 'red') #+
# geom_vline(xintercept = t_small$keydown / secres, color = 'green') +
#   geom_vline(xintercept = t_small$keyup / secres, color = 'grey') +
#   geom_vline(xintercept = t_small$disp_start / secres, color = 'darkgreen') +
#   geom_vline(xintercept = t_small$disp_end / secres, color = 'black')
# plotly::ggplotly(p)

# colrs = viridis::viridis(3, end = .85)
# p = ggplot(data = allsampls, aes(x = sample, y = values, group = 1)) +
#   geom_line() + scale_color_manual(values = colrs) + facet_wrap(vars(version))
