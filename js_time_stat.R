# libs ----
library("neatStats")
library("ggplot2")
#
# 20,000 samples per second

setwd(path_neat(""))
filenames = list.files(pattern = "^disptime.*.csv$")
secres = 20000

allsampls = data.frame()
for (fname in filenames) {
  # fname = 'disptime_Windows_Chrome_black_20210527112950_pilot1a.csv'
  # fname = 'disptime_Windows_Chrome_black_20210527125457_pilot1b.csv'
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
  last_end = secres
  trialnum = 0
  for (index in secres:(length(sampls$triggers) - secres)) {
    if (index > (last_end + secres * 0.8) &&
        sampls$triggers[index] == 1) {
      if (mean(sampls$triggers[(index - 10):(index - 1)]) < 0.5 &&
          mean(sampls$triggers[(index):(index + secres * 0.1)]) > 0.9 &&
          mean(sampls$triggers[(index + secres * 0.2):(index + secres * 0.4)]) < 0.1) {
        if (nrow(trial_data) > (nrow(rt_data))) {
          break
        }
        for (endindx in (index + secres * 0.08):(index + secres * 0.18)) {
          if (sampls$triggers[endindx] == 0 &&
              mean(sampls$triggers[(endindx):(endindx + secres * 0.01)]) < 0.1) {
            # get display info
            disp_start = NA
            disp_end = NA
            for (start_i in (index):(index + secres * 0.2)) {
              if (sampls$values[start_i] > cutoff_low &&
                  mean(sampls$values[(start_i):(start_i + secres * 0.005)]) > cutoff_low &&
                  mean(sampls$values[(start_i + secres * 0.005):(start_i + secres * 0.015)]) > cutoff_upp) {
                if (start_i < (index + secres * 0.005)) {
                  stop('suspiciously fast change... ')
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
            trialnum = trialnum + 1
            last_end = endindx
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


}

## sanity checks

# unique(sampls$triggers)
# ggpubr::gghistogram((c(trial_data$keydown[-1]) -
#                      trial_data$keydown[-length(trial_data$keydown)]) / secres)


d_small = sampls[sampls$sample > 0 & sampls$sample < 30,]
p = ggplot(data = d_small, aes(x = sample, y = values, group = 1)) +
  geom_line(color = 'blue') +
  geom_line(aes(y = triggers), color = 'red')
p

colrs = viridis::viridis(3, end = .85)
p = ggplot(data = allsampls, aes(x = sample, y = values, group = 1)) +
  geom_line() + scale_color_manual(values = colrs) + facet_wrap(vars(version))
plotly::ggplotly(p)
