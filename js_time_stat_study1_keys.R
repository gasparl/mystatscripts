# libs ----
library("neatStats")
library("ggplot2")
library('dplyr')
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

setwd(path_neat(""))

dat_py = readRDS("2021_disp_time_aggr_python.rds")
#dat_py$study = 'python'
dat_stud = readRDS("2021_disp_time_aggr_study1.rds")
dat_stud$study = NULL
full_data = rbind(dat_py, dat_stud)
full_data$OS = ifelse(full_data$OS != 'Windows',
                      full_data$OS,
                      ifelse(substr(full_data$datetime, 6, 9) == '0929', "Win (B)", "Win (A)"))
full_data$OS[full_data$OS == "Mac OS X"] = "MacOS"
full_data$Browser[full_data$Browser == "Microsoft Edge"] = "Edge"

full_data$os_browser = paste(full_data$OS, full_data$Browser, sep = ' / ')
full_data$os_browser[full_data$os_browser == "NA / psychopy"] = "PsychoPy"

full_data$os_browser = trimws(full_data$os_browser)
full_data$os_browser = ifelse(
  grepl("PsychoP", full_data$os_browser),
  full_data$os_browser,
  paste0('  ', full_data$os_browser, '')
)
full_data$os_browser = ifelse(
  grepl("Win (A)", full_data$os_browser, fixed = TRUE),
  full_data$os_browser,
  paste0(' ', full_data$os_browser, '')
)

full_data$method[full_data$method == "none"] = "direct"
full_data$method[full_data$method == "rAF_single"] = "RAF 1"
full_data$method[full_data$method == "rAF_double"] = "RAF 2"
full_data$method[full_data$method == "rAF_loop"] = "RAF loop"

# tnums = unique(full_data$trial_number)
# full_data$trial_number = factor(full_data$trial_number, levels = tnums[order(nchar(tnums), tnums)])

# full_data = full_data[full_data$file == "disptime_plain_Windows_Chrome_white_2021_0929_1218.csv",]

full_data$keydelay = full_data$js_input - full_data$keydown/ msecres

# raw standard
# full_data = as.data.frame(full_data %>% group_by(file) %>% mutate(keydelay = keydelay - mean(keydelay)) %>% ungroup())
full_data = as.data.frame(
  full_data %>% group_by(file) %>%
    mutate(
      keydelay = (keydelay - mean(keydelay)) - keydown * lm((keydelay - mean(keydelay)) ~ keydown)$coefficients[2] - lm((keydelay - mean(keydelay)) ~ keydown)$coefficients[1]
    )
  %>% ungroup()
)

# ggpubr::ggdotplot(full_data, 'trial_number', 'keydelay')
# unique(full_data$file)

# dot_data = full_data[full_data$file == 'disptime_plain_Windows_Chrome_white_2021_0929_1218.csv',]
dot_data = full_data#[as.numeric(as.character(full_data$trial_number)) < 50,]
dotplot = ggpubr::ggdotplot(
  dot_data, # fill = "method",
  'trial_number',
  'keydelay',
  xlab = 'Time (trial number)',
  ylab = 'Relative delay (ms)',
  facet.by = c('os_browser'),
  #facet.by = c('file'),
) + scale_x_discrete(
  #breaks = c(0, 250, 500, 1000),
  breaks = c(250, 500, 750)
) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#d5d5d5"),
    panel.grid.minor.y = element_line(color = "#d5d5d5")
  ) + theme(
    strip.background = element_blank(),
    legend.position = "top",
    legend.margin = margin(0),
    legend.text = element_text(margin = margin(r = 7, unit = "pt"))
  )

#[full_data$keydelay <5,]
ggplot(full_data, aes(x = keydelay)) +
  geom_histogram(aes(color = NULL), bins = 30)  +
  theme_bw() +
  # facet_wrap(vars(os_browser)) +
  facet_wrap(vars(method)) +
  theme(legend.position = "top")

# STATS

colrs = viridis::viridis(6, end = 0.85)

dat_stat = full_data
dat_statpy = dat_stat[dat_stat$Browser == 'psychopy', ]
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
    the_delays = dat_comp$keydelay[!is.na(dat_comp$keydelay)]
    d1mad = DescTools::MADCI(the_delays, na.rm = TRUE)
    d1mad[d1mad == Inf | d1mad == -Inf] = NA
    nextrow2 =
      list(
        os = the_os,
        browser = the_brws,
        os_browser = dat_comp$os_browser[1],
        sd = sd(the_delays),
        mad = as.numeric(d1mad['mad']),
        sd_lo = sd(the_delays) * sd_c_l(length(the_delays)),
        sd_up = sd(the_delays) * sd_c_u(length(the_delays)),
        mad_lo = as.numeric(d1mad['lwr.ci']),
        mad_up = as.numeric(d1mad['upr.ci']),
        reaction_median = median(the_delays),
        reaction_mad = mad(the_delays),
        reaction_mean = mean(the_delays),
        reaction_m_ci = neatStats::mean_ci(the_delays)
      )
    final_dat = rbind(final_dat, nextrow2)

    if (the_brws != 'psychopy') {
      res = var_tests(
        "keydelay",
        group_by = 'os_browser',
        rbind(dat_comp, dat_statpy),
        hush = T
      )
      nextrow1 =
        list(
          os = the_os,
          browser = the_brws,
          os_browser = dat_comp$os_browser[1],
          p_BF = res$p_BF,
          p_FK = res$p_FK,
          ypos = max(nextrow2$sd, nextrow2$mad)
        )
      tests_dat = rbind(tests_dat, nextrow1)
    }
  }
}

tests_dat$p_BFadj = p.adjust(tests_dat$p_BF, "BH")
tests_dat$p_FKadj = p.adjust(tests_dat$p_FK, "BH")
# tests_dat$p_BFadj2 = ro(tests_dat$p_BFadj, 4)

palcolors = viridis::viridis(2, begin = 0, end = 0.75)

# [final_dat$browser == "Safari",]
p_keydlys = ggplot2::ggplot(data = final_dat,
                         aes(x = .data$os_browser,
                             y = .data$sd,
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
    ymin = .data$sd_lo,
    ymax = .data$sd_up,
    width = 0.2
  )) +
  geom_line(aes(y = .data$mad, color = 'MAD')) +
  geom_point(size = 0.6, aes(y = .data$mad, color = 'MAD')) + geom_errorbar(aes(
    ymin = .data$mad_lo,
    ymax = .data$mad_up,
    width = 0.2,
    color = 'MAD'
  )) + theme_bw() +
  labs(x = 'OS / browser', y = "Deviation (ms)") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#d5d5d5"),
    panel.grid.minor.x = element_line(color = "#d5d5d5")
  ) + coord_flip() +
  theme(strip.background = element_blank(),
        legend.position = "top",
        legend.margin = margin(0),
        legend.text = element_text(margin = margin(r = 7, unit = "pt")))

# p_keydlys

tests_dat$group1 = "PsychoPy"
tests_dat$group2 = tests_dat$os_browser
tests_dat$pBF = ifelse(tests_dat$p_BFadj < 0.0001,
                       '⁎⁎⁎',
                       ifelse(
                         tests_dat$p_BFadj < 0.001,
                         '⁎⁎',
                         ifelse(tests_dat$p_BFadj < 0.01, '⁎', '-')
                       ))
tests_dat$pFK = ifelse(tests_dat$p_FKadj < 0.0001,
                       '⁎⁎⁎',
                       ifelse(
                         tests_dat$p_FKadj < 0.001,
                         '⁎⁎',
                         ifelse(tests_dat$p_FKadj < 0.01, '⁎', '-')
                       ))
tests_dat$p = paste0('(', tests_dat$pBF, ' ', tests_dat$pFK, ')')
tests_dat$y.position = tests_dat$ypos + 0.1

p_keydlys + ggpubr::stat_pvalue_manual(data = tests_dat,
                                       remove.bracket = TRUE,
                                       label = NULL)  +
  scale_y_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    # minor_breaks = c(0.25, 0.75),
    limits = c(NA, 1.02)
  )

final_dat$os_browser = trimws(final_dat$os_browser)
tests_dat$os_browser = trimws(tests_dat$os_browser)
write.table(final_dat, "js_disptime_study1_keypress_stats_descriptives.txt", quote = F, row.names = F, sep="\t")
write.table(tests_dat, "js_disptime_study1_keypress_stats_tests.txt", quote = F, row.names = F, sep="\t")
