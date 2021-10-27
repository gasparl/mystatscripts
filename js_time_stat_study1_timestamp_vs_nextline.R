# libs ----
library("neatStats")
library("ggplot2")
sd_c_l = function(n) {
  sqrt((n - 1) / qchisq(0.05 / 2, n - 1, lower.tail = FALSE))
}
sd_c_u  = function(n) {
  sqrt((n - 1) / qchisq(0.05 / 2, n - 1, lower.tail = TRUE))
}
#
# 20,000 samples per second

secres = 20000
msecres = secres / 1000
colrs = viridis::viridis(5, end = 0.85)

setwd(path_neat(""))


#full_data = readRDS("2021_disp_time_aggr_python.rds")
full_data = readRDS("2021_disp_time_aggr_study1.rds")
full_data$OS = ifelse(full_data$OS != 'Windows',
                      full_data$OS,
                      ifelse(substr(full_data$datetime, 6, 9) == '0929', "Win (B)", "Win (A)"))
full_data$OS[full_data$OS == "Mac OS X"] = "macOS"
full_data$Browser[full_data$Browser == "Microsoft Edge"] = "Edge"

# full_data = full_data[full_data$file == "disptime_plain_Windows_Chrome_white_2021_0821_1220.csv",]

#full_data = full_data[full_data$study == 'plain',]
#full_data = full_data[full_data$study == 'image',]

#full_data$js_start = full_data$js_start_stamp
full_data$js_end = full_data$js_end_stamp

full_data = reshape(
  full_data,
  varying = c("js_start_stamp", "js_start_nextline"),
  v.names = "js_start",
  timevar = "timer",
  times = c("timestamp", "direct"),
  direction = "long"
)

full_data$d1_ext = (full_data$disp_start - full_data$keydown) / msecres
full_data$d1_js = full_data$js_start - full_data$js_input
full_data$d1_diff = full_data$d1_js - full_data$d1_ext
full_data$d1_key = full_data$d1_diff - full_data$d1_js

full_data$d2_ext = (full_data$disp_end - full_data$disp_start) / msecres
#full_data$d2_ext[is.na(full_data$d2_ext)] = 0
full_data$d2_js = full_data$js_end - full_data$js_start

full_data$d2_diff = full_data$d2_js - full_data$d2_ext
if (mean(abs(full_data$d2_diff[!is.na(full_data$d2_diff)])) > 25) {
  stop('inconsistent timings: ', mean(abs(full_data$d2_diff)))
}

full_data$d3_diff = full_data$duration - full_data$d2_ext

full_data = full_data[full_data$method %in% c("rAF_double",
                                              "rAF_loop",
                                              "rAF_single"),]

full_data$method[full_data$method == "none"] = "direct"
full_data$method[full_data$method == "rAF_single"] = "RAF 1"
full_data$method[full_data$method == "rAF_double"] = "RAF 2"
full_data$method[full_data$method == "rAF_loop"] = "RAF loop"
full_data$method = paste(full_data$method, full_data$timer, sep = ' / ')
full_data$os_browser = paste(full_data$OS, full_data$Browser, sep = ' / ')
full_data$os_browser = ifelse(
  grepl("Win ", full_data$os_browser),
  paste0('  ', full_data$os_browser, '  '),
  full_data$os_browser
)
full_data$os_browser = ifelse(
  grepl("macOS", full_data$os_browser),
  paste0(' ', full_data$os_browser, ' '),
  full_data$os_browser
)

# PLOTTING ####

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

## DIRECT VS TIMESTAMP COMPARISONS
unique(pdata$OS)
# "Linux"   "macOS"   "Win (A)" "Win (B)"
ggplot(pdata[pdata$OS == 'macOS',], aes(x = d1_diff)) + ggtitle(n_dict['d1_diff']) + xlab(n_dict['diff']) +
  geom_histogram(aes(color = NULL, fill = OS), binwidth = binw)  +
  theme_bw() + scale_fill_manual(values = colrs) + facet_wrap(vars(method, os_browser)) +
  theme(legend.position = "top")

#pdata = pdata[pdata$type == 'img_large', ]
#pdata = pdata[pdata$method %in% c('rAF_double', 'rAF_loop'),]

print('real from keydown to display')
ggplot(pdata, aes(x = d1_ext)) + ggtitle(n_dict['d1_ext']) + xlab(n_dict['dur']) +
  geom_histogram(aes(color = NULL, fill = OS), binwidth = binw)  +
  theme_bw() + scale_fill_manual(values = colrs) + facet_wrap(vars(method, Browser)) +
  theme(legend.position = "top")

print('JS from keydown to display')
ggplot(pdata, aes(x = d1_js)) + ggtitle(n_dict['d1_js']) + xlab(n_dict['dur']) +
  geom_histogram(aes(color = NULL, fill = OS), binwidth = binw)  +
  theme_bw() + scale_fill_manual(values = colrs) + facet_wrap(vars(method, Browser)) +
  theme(legend.position = "top")

print('DIFF from keydown to display')
# [pdata$d1_diff > -90 & pdata$OS == 'Windows',]
# [pdata$d1_diff > -70 & pdata$method == 'RAF 1',]
# [pdata$background == 'white',]
ggplot(pdata[pdata$OS == 'Win (A)',], aes(x = d1_diff)) + ggtitle(n_dict['d1_diff']) + xlab(n_dict['diff']) +
  geom_histogram(aes(color = NULL, fill = OS), binwidth = binw)  +
  theme_bw() + scale_fill_manual(values = colrs) + facet_wrap(vars(method, os_browser)) +
  theme(legend.position = "top")

# [full_data$d2_ext != 0,]
# [pdata$d2_diff < 200,]
# [pdata$OS == "Win (B)",]
print('DIFF duration (from display to next display)')
ggplot(pdata, aes(x = d2_diff)) + ggtitle(n_dict['d2_diff']) + xlab(n_dict['diff']) +
  geom_histogram(aes(color = NULL, fill = OS), binwidth = binw)  +
  theme_bw() + scale_fill_manual(values = colrs) + facet_wrap(vars(method, Browser)) +
  theme(legend.position = "top")

# [pdata$d3_diff < 300,]
print('DIFF expected minus real duration')
ggplot(pdata, aes(x = d3_diff)) + ggtitle(n_dict['d3_diff']) + xlab(n_dict['dur']) +
  geom_histogram(aes(color = NULL, fill = OS), binwidth = binw)  +
  theme_bw() + scale_fill_manual(values = colrs) + facet_wrap(vars(method, Browser)) +
  theme(legend.position = "top")

ggplot(pdata, aes(x = d1_ext)) + ggtitle(n_dict['d1_ext']) + xlab(n_dict['dur']) +
  geom_histogram(aes(color = NULL, fill = OS), binwidth = binw)  +
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


## missing frames
dcheck = full_data
dcheck = dcheck[is.na(dcheck$d2_diff),]
ggplot(dcheck, aes(x = duration)) + xlab('Expected durations') +
  geom_histogram(aes(color = NULL), bins = 30) + scale_fill_manual(values = colrs)   +
  theme_bw() + facet_wrap(vars(method, Browser, type, background))

# SIGNIFICANCE TESTING ####

dat_stat = full_data
comps = list(c('RAF 1 / timestamp', 'RAF 1 / direct'),
             c('RAF 2 / timestamp', 'RAF 2 / direct'),
             c('RAF loop / timestamp', 'RAF loop / direct'))
final_dat = data.frame()
tests_dat = data.frame()
for (the_os in unique(dat_stat$OS)) {
  for (the_brws in unique(dat_stat$Browser)) {
    dat_comp = dat_stat[dat_stat$OS == the_os &
                           dat_stat$Browser == the_brws,]
    if (nrow(dat_comp) < 1) {
      message('skipped ', the_os, ' / ', the_brws)
      next
    }
    for (comp_pair in comps) {
      dat_comp2 = dat_comp[dat_comp$method %in% comp_pair, ]
      res1 = var_tests("d1_diff", group_by = 'method', dat_comp2, hush = T)
      res2 = var_tests("d2_diff", group_by = 'method', dat_comp2, hush = T)
      nextrow1 =
        list(
          os = the_os,
          browser = the_brws,
          os_browser = dat_comp2$os_browser[1],
          method1 = comp_pair[1],
          method2 = comp_pair[2],
          p_BF1 = res1$p_BF,
          p_FK1 = res1$p_FK,
          p_BF2 = res2$p_BF,
          p_FK2 = res2$p_FK,
          p_wilcox = wilcox.test(dat_comp2$d1_ext[dat_comp2$method == comp_pair[1]],
                                 dat_comp2$d1_ext[dat_comp2$method == comp_pair[2]])$p.value
        )
      tests_dat = rbind(tests_dat, nextrow1)
    }
    for (the_meth in unique(dat_comp$method)) {
      dat_comp3 = dat_comp[dat_comp$method == the_meth, ]
      d1_dif = dat_comp3$d1_diff[!is.na(dat_comp3$d1_diff)]
      d2_dif = dat_comp3$d2_diff[!is.na(dat_comp3$d2_diff)]
      d1_ex = dat_comp3$d1_ext[!is.na(dat_comp3$d1_ext)]
      d1mad = DescTools::MADCI(d1_dif, na.rm = TRUE)
      d2mad = DescTools::MADCI(d2_dif, na.rm = TRUE)
      d1mad[d1mad == Inf | d1mad == -Inf] = NA
      d2mad[d2mad == Inf | d2mad == -Inf] = NA
      nextrow2 =
        list(
          os = the_os,
          browser = the_brws,
          os_browser = dat_comp3$os_browser[1],
          method = the_meth,
          median1 = median(d1_dif),
          sd1 = sd(d1_dif),
          mad1 = as.numeric(d1mad['mad']),
          median2 = median(d2_dif),
          sd2 = sd(d2_dif),
          mad2 = as.numeric(d2mad['mad']),
          sd1_lo = sd(d1_dif) * sd_c_l(length(d1_dif)),
          sd1_up = sd(d1_dif) * sd_c_u(length(d1_dif)),
          sd2_lo = sd(d2_dif) * sd_c_l(length(d2_dif)),
          sd2_up = sd(d2_dif) * sd_c_u(length(d2_dif)),
          mad1_lo = as.numeric(d1mad['lwr.ci']),
          mad1_up = as.numeric(d1mad['upr.ci']),
          mad2_lo = as.numeric(d2mad['lwr.ci']),
          mad2_up = as.numeric(d2mad['upr.ci']),
          reaction_median = median(d1_ex),
          reaction_mad = mad(d1_ex),
          reaction_mean = mean(d1_ex),
          reaction_m_ci = neatStats::mean_ci(d1_ex)
        )
      final_dat = rbind(final_dat, nextrow2)
    }
  }
}

tests_dat$p_BF1adj = p.adjust(tests_dat$p_BF1, "BH")
tests_dat$p_BF2adj = p.adjust(tests_dat$p_BF2, "BH")
tests_dat$p_FK1adj = p.adjust(tests_dat$p_FK1, "BH")
tests_dat$p_FK2adj = p.adjust(tests_dat$p_FK2, "BH")
tests_dat$p_wilcox_adj = p.adjust(tests_dat$p_wilcox, "BH")
# tests_dat$p_wilcox_adj2 = ro(tests_dat$p_wilcox_adj, 4)

palcolors = viridis::viridis(2, begin = 0, end = 0.75)

# DIFF 1
# [final_dat$browser == "Safari",]
p_dif1 = ggplot2::ggplot(data = final_dat,
                aes(x = .data$method,
                    y = .data$sd1,
                    group = 1, color = 'SD'))  +
  scale_color_manual(
    "",
    breaks = c("SD", "MAD"),
    values = c(
      "SD" = palcolors[1],
      "MAD" = palcolors[2]
    )
  )+
  geom_line() +
  geom_point(size = 0.6) + geom_errorbar(aes(
    ymin = .data$sd1_lo,
    ymax = .data$sd1_up,
    width = 0.2
  )) +
  geom_line(aes(y = .data$mad1, color = 'MAD')) +
  geom_point(size = 0.6, aes(y = .data$mad1, color = 'MAD')) + geom_errorbar(aes(
    ymin = .data$mad1_lo,
    ymax = .data$mad1_up,
    width = 0.2,
    color = 'MAD'
  )) + theme_bw() +
  labs(x = 'OS / browser', y = "Deviation (ms)") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#d5d5d5"),
    panel.grid.minor.y = element_line(color = "#d5d5d5")
  ) + facet_wrap(~ .data$os_browser, ncol = 3) +
  theme(strip.background = element_blank(),
        legend.position = "top",
        legend.margin = margin(0),
        legend.text = element_text(margin = margin(r = 7, unit = "pt")))

tests_dat$group1 = tests_dat$method1
tests_dat$group2 = tests_dat$method2
tests_dat$pBF = ifelse(tests_dat$p_BF1adj < 0.0001,
                       '⁎⁎⁎',
                       ifelse(
                         tests_dat$p_BF1adj < 0.001,
                         '⁎⁎',
                         ifelse(tests_dat$p_BF1adj < 0.01, '⁎', '-')
                       ))
tests_dat$pFK = ifelse(tests_dat$p_FK1adj < 0.0001,
                       '⁎⁎⁎',
                       ifelse(
                         tests_dat$p_FK1adj < 0.001,
                         '⁎⁎',
                         ifelse(tests_dat$p_FK1adj < 0.01, '⁎', '-')
                       ))
tests_dat$p = paste(tests_dat$pBF, tests_dat$pFK, sep = ' ')
tests_dat$y.position = 11

p_dif1 + ggpubr::stat_pvalue_manual(data = tests_dat, label = NULL)  +
  scale_y_continuous(
    breaks = c(0, 5, 10),
    limits = c(NA, 25),
    minor_breaks = c(2.5, 7.5)
  ) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# DIFF 2

ggplot2::ggplot(data = final_dat,
                aes(x = .data$method,
                    y = .data$sd2,
                    group = 1, color = 'SD'))  +
  scale_color_manual(
    "",
    breaks = c("SD", "MAD"),
    values = c(
      "SD" = palcolors[1],
      "MAD" = palcolors[2]
    )
  )+
  geom_line() +
  geom_point(size = 0.6) + geom_errorbar(aes(
    ymin = .data$sd2_lo,
    ymax = .data$sd2_up,
    width = 0.2
  )) +
  geom_line(aes(y = .data$mad2, color = 'MAD')) +
  geom_point(size = 0.6, aes(y = .data$mad2, color = 'MAD')) + geom_errorbar(aes(
    ymin = .data$mad2_lo,
    ymax = .data$mad2_up,
    width = 0.2,
    color = 'MAD'
  )) + theme_bw() +
  labs(x = 'OS / browser', y = "Deviation (ms)") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#d5d5d5"),
    panel.grid.minor.y = element_line(color = "#d5d5d5")
  ) + facet_wrap(~ .data$os_browser, ncol = 3) +
  theme(strip.background = element_blank(),
        legend.position = "top",
        legend.margin = margin(0),
        legend.text = element_text(margin = margin(r = 7, unit = "pt")))

# Reaction time

ggplot2::ggplot(data = final_dat,
                         aes(x = .data$method,
                             y = .data$reaction_mean,
                             group = 1, color = 'Mean'))  +
  scale_color_manual(
    "",
    breaks = c("Mean", "Median"),
    values = c(
      "Mean" = palcolors[1],
      "Median" = palcolors[2]
    )
  )+
  geom_line() +
  geom_point(size = 0.6) + geom_errorbar(aes(
    ymin = .data$reaction_mean - .data$reaction_m_ci,
    ymax = .data$reaction_mean + .data$reaction_m_ci,
    width = 0.2
  )) +
  geom_line(aes(y = .data$reaction_median, color = 'Median')) +
  geom_point(size = 0.6, aes(y = .data$reaction_median, color = 'Median')) + theme_bw() +
  # geom_errorbar(aes(
  #   ymin = .data$reaction_median - .data$reaction_mad,
  #   ymax = .data$reaction_median + .data$reaction_mad,
  #   width = 0.2,
  #   color = 'Median'
  # )) +
  labs(x = 'OS / browser', y = "Reaction time (ms)") +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#d5d5d5"),
    panel.grid.minor.y = element_line(color = "#d5d5d5")
  ) + facet_wrap(~ .data$os_browser, ncol = 3) +
  theme(strip.background = element_blank(),
        legend.position = "top",
        legend.margin = margin(0),
        legend.text = element_text(margin = margin(r = 7, unit = "pt")))
