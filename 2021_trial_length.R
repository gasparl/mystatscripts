library('neatStats')
library('ggplot2')
range_scl =  function(x, newMin, newMax) {
  (x - min(x)) / (max(x) - min(x)) * (newMax - newMin) + newMin
}

# Set working directory
setwd(path_neat())


cit_trials = utils::read.table(
  "cit_trialpreds_meta.txt",
  sep = "\t",
  header = TRUE,
  fill = TRUE,
  quote = "\"",
  stringsAsFactors = FALSE
)

compare = readRDS("half_predictors_meta.rds")

cit_trials = cit_trials[cit_trials$subject_id %in% compare$id,]



# dat9 = trials_guilty[trials_guilty$dataset == 'dataset 9',]
# for (sid in unique(dat9$subject_id)) {
#   subj = dat9[dat9$subject_id == sid, ]
#   print(subj$trial_number)
# }

# for (dnum in unique(cit_trials$dataset)) {
#   maxtrial = max(cit_trials$trial_number[cit_trials$dataset == dnum])
#   for (sid in unique(cit_trials$subject_id[cit_trials$dataset == dnum])) {
#     cit_trials$rt[cit_trials$dataset == dnum &
#                     cit_trials$subject_id == sid] =
#       range_scl(cit_trials$rt[cit_trials$dataset == dnum &
#                                 cit_trials$subject_id == sid], 1, maxtrial)
#   }
# }


#cit_trials = cit_trials[seq(1, nrow(cit_trials), 5),]

cit_trials = excl_neat(cit_trials, (correct == 1), group_by = 'dataset')

uniqs = unique(cit_trials$dataset)
cit_trials$dataset = factor(cit_trials$dataset, levels = uniqs[order(nchar(uniqs), uniqs)])

# trials_full = cit_trials
# trials_full$valid_trial = trials_full$correct
# trials_full$guilt = ifelse(cit_trials$outcome == 1, 'guilty', 'innocent')
# trials_full$item_type[trials_full$item_type == 'irrelevant'] = 'control'
# trials_full$item_type = factor(trials_full$item_type, levels = c('probe', 'control', 'target'))
# names(trials_full)[names(trials_full) == 'rt'] = 'rt_start'
# names(trials_full)[names(trials_full) == 'item_type'] = 'stim_type'

# saveRDS(trials_full, 'meta_all_trials.rds')

trials_guilty = cit_trials[cit_trials$outcome == 1,]
trials_guilty$item_type[trials_guilty$item_type == 'irrelevant'] = 'control'
trials_guilty$item_type = factor(trials_guilty$item_type, levels = c('probe', 'control', 'target'))

# saveRDS(trials_guilty, 'meta_guilty_trials.rds')

colrs = viridis::viridis(3, end = 0.85)
plot_items = ggplot(data = trials_guilty, aes(x = trial_number, y = rt)) +
  geom_smooth(aes(color = item_type, fill = item_type)) +
  facet_wrap(vars(dataset)) +
  theme_bw() + theme(strip.background = element_blank(), plot.title = element_text(hjust = 0.5),
        strip.text = element_text(face = 'plain', size = 12), legend.position = "bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = colrs) +
  scale_fill_manual(values = colrs) +
  ylab('Response time') + xlab('Trial number') + ggtitle('RTs per Item Type')



####

pltdat <- ggplot_build(plot_items)[['data']][[1]]

df_diffs = data.frame()
for (panl in unique(pltdat$PANEL)) {
  subpltdat = pltdat[pltdat$PANEL == panl,]
  dat_probe <-
    subpltdat[subpltdat$group == 2,] # Extract info for group 1
  dat_irr <-
    subpltdat[subpltdat$group == 1,] # Extract info for group 2
  xlimit.inf <- max(min(dat_probe$x), min(dat_irr$x)) # Get the minimum X the two smoothed data have in common
  xlimit.sup <- min(max(dat_probe$x), max(dat_irr$x)) # Get the maximum X
  xseq <- seq(xlimit.inf, xlimit.sup, 12) # Sequence of X value (you can use bigger/smaller step size)
  # Based on data from group 1 and group 2, interpolates linearly for all the values in `xseq`
  y_probe <- approx(x = dat_probe$x, y = dat_probe$y, xout = xseq)
  y_irr <- approx(x = dat_irr$x, y = dat_irr$y, xout = xseq)

  difference <-
    data.frame(
      x = xseq,
      rtdiff = abs(y_probe$y - y_irr$y),
      dataset = rep(paste('dataset', panl), length(xseq))
    ) # Compute the difference
  df_diffs = rbind(df_diffs, difference)
}

plot_diffs = ggplot(data = df_diffs, aes(x = x, y = rtdiff, color = dataset, fill = dataset)) +
  geom_smooth(se = FALSE, color = '#00004d') + facet_wrap(vars(dataset)) +
  theme_bw() + theme(strip.background = element_blank(), plot.title = element_text(hjust = 0.5),
                     strip.text = element_text(face = 'plain', size = 12), legend.position = "none") +
  ylab(NULL) + xlab('Trial number') + ggtitle('Probe-Control RT Differences')
  #scale_color_manual(values = viridis::viridis(12, end = 0.99)) +
  #scale_fill_manual(values = viridis::viridis(12, end = 0.85))

plot_items
plot_diffs


ggpubr::ggarrange(plot_items, plot_diffs,
                  ncol = 2, common.legend = T, legend = 'bottom')



