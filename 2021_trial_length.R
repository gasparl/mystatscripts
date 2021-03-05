library('neatStats')
library('ggplot2')

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

#cit_trials = cit_trials[seq(1, nrow(cit_trials), 5),]

cit_trials = cit_trials[cit_trials$correct = 1,]
uniqs = unique(cit_trials$dataset)
cit_trials$dataset = factor(cit_trials$dataset, levels = uniqs[order(nchar(uniqs), uniqs)])

trials_guilty = cit_trials[cit_trials$outcome == 1, ]

colrs = viridis::viridis(3, end = 0.85)
plot_items = ggplot(data = trials_guilty, aes(x = trial_number, y = rt)) +
  geom_smooth(aes(color = item_type, fill = item_type)) + facet_wrap(vars(dataset)) +
  theme_bw() + theme(strip.background = element_blank(),
        strip.text = element_text(face = 'bold', size = 12)) +
  scale_color_manual(values = colrs) +
  scale_fill_manual(values = colrs)


####
plot_items
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
  xseq <- seq(xlimit.inf, xlimit.sup, 25) # Sequence of X value (you can use bigger/smaller step size)
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

ggplot(data = df_diffs, aes(x = x, y = rtdiff)) +
  geom_smooth() + facet_wrap(vars(dataset)) +
  theme_bw() + theme(strip.background = element_blank(),
                     strip.text = element_text(face = 'bold', size = 12)) +
  scale_color_manual(values = colrs) +
  scale_fill_manual(values = colrs)



