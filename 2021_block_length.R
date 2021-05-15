library('neatStats')
library('metafor')
library('ggplot2')

# Set working directory
setwd(path_neat())

main_preds = readRDS("half_predictors_meta.rds")
aucs = readRDS("half_aucs_meta.rds")

for (dset in enum(unique(main_preds$dataset))) {
  cat(dset, '; ')
  set_preds = main_preds[main_preds$dataset == dset[2], ]
  rt_diff = neatStats::aggr_neat(
    dat = set_preds,
    values = rt_mean_diff,
    method = mean,
    group_by = c("cond"),
    prefix = "m_rt_diff"
  )
  rt_diff_h1 = neatStats::aggr_neat(
    dat = set_preds,
    values = rt_mean_h1_diff,
    method = mean,
    group_by = c("cond"),
    prefix = "m_rt_diff_h1"
  )
  rt_diff_h2 = neatStats::aggr_neat(
    dat = set_preds,
    values = rt_mean_h2_diff,
    method = mean,
    group_by = c("cond"),
    prefix = "m_rt_diff_h2"
  )
  sd_rt_diff = neatStats::aggr_neat(
    dat = set_preds,
    values = rt_mean_diff,
    method = sd,
    group_by = c("cond"),
    prefix = "sd_rt_diff"
  )
  sd_rt_diff_h1 = neatStats::aggr_neat(
    dat = set_preds,
    values = rt_mean_h1_diff,
    method = sd,
    group_by = c("cond"),
    prefix = "sd_rt_diff_h1"
  )
  sd_rt_diff_h2 = neatStats::aggr_neat(
    dat = set_preds,
    values = rt_mean_h2_diff,
    method = sd,
    group_by = c("cond"),
    prefix = "sd_rt_diff_h2"
  )
  rbind_loop(
    meta_dat,
    dataset = set_preds$dataset[1],
    study = set_preds$study[1],
    cit_type = set_preds$multiple_single[1],
    rt_diff,
    rt_diff_h1,
    rt_diff_h2,
    sd_rt_diff,
    sd_rt_diff_h1,
    sd_rt_diff_h2,
    r_h12 = stats::cor(set_preds$rt_mean_h1_diff[set_preds$cond == 1],
                       set_preds$rt_mean_h2_diff[set_preds$cond == 1]),
    n_g = length(set_preds$rt_mean_h1_diff[set_preds$cond == 1])
  )
}


meta_dat$cit_type[meta_dat$cit_type == 1] = '(MP)'
meta_dat$cit_type[meta_dat$cit_type == 0] = '(SP)'
meta_dat$cit_type[meta_dat$cit_type == 2] = '(SPF)'
meta_dat$title = paste(meta_dat$dataset, meta_dat$study,  meta_dat$cit_type)
meta_dat$title = gsub('Exp', 'Exp. ', meta_dat$title)
meta_dat$title = gsub('Geven, Ben-Shakhar, Kindt, & Verschuere',
                      'Geven et al.',
                      meta_dat$title)
meta_dat$title = gsub('Lukács, Kleinberg, & Verschuere',
                      'Lukács et al.',
                      meta_dat$title)
meta_dat$title = gsub('Verschuere, Kleinberg, & Theocharidou',
                      'Verschuere, Kleinberg, et al.',
                      meta_dat$title)
meta_dat$title = gsub('Kleinberg & Verschuere',
                      'Kleinberg et al.',
                      meta_dat$title)
meta_dat$title = gsub('Noordraven & Verschuere',
                      'Noordraven et al.',
                      meta_dat$title)

meta_dat <-
  escalc(
    measure = 'SMCR',
    m1i = m_rt_diff_h1_1,
    m2i = m_rt_diff_h2_1,
    sd1i = sd_rt_diff_h1_1,
    sd2i = sd_rt_diff_h2_1,
    ni = n_g,
    ri = r_h12,
    data = meta_dat,
    slab = dataset
  )
res = rma(yi, vi, data = meta_dat)
res
confint(res)
#predict(res, digits=3)

forest(res,
       slab = meta_dat$title, mlab = 'Random Effects Model for All Studies',
       fonts = 'serif', #showweight=TRUE,
       xlim = c(1.7,-3))


# t_neat(meta_dat$m_rt_diff_h1_1,
#        meta_dat$m_rt_diff_h2_1, pair = T)

# FIG
#
twocolrs = viridis::viridis(2, end = 0.5)
fig_dat = stats::reshape(
  meta_dat,
  direction = 'long',
  varying = c("m_rt_diff_h1_1", "m_rt_diff_h2_1"),
  idvar = c("dataset"),
  timevar = "version",
  v.names = "rt_diff",
  times = c("m_rt_diff_h1_1", "m_rt_diff_h2_1"),
)
fig_dat$ci_dist = ifelse(
  fig_dat$version == 'm_rt_diff_h1_1',
  meta_dat$sd_rt_diff_h1_1 /
    sqrt(meta_dat$n_g) * stats::qnorm(0.975),
  meta_dat$sd_rt_diff_h2_1 /
    sqrt(meta_dat$n_g) * stats::qnorm(0.975)
)

fig_dat$version[fig_dat$version == 'm_rt_diff_h1_1'] = 'First half'
fig_dat$version[fig_dat$version == 'm_rt_diff_h2_1'] = 'Second half'
fig_dat$dataset = as.factor(fig_dat$dataset)
ggplot2::ggplot(data = fig_dat, aes(x = dataset,
                                    y = rt_diff,
                                    fill = version)) +
  geom_bar(stat = "identity",
           color = "black",
           position = position_dodge(0.9)) +
  scale_fill_manual(values = c(twocolrs[1], twocolrs[2]),name = NULL) +
  geom_errorbar(aes(
    ymin = fig_dat$rt_diff - fig_dat$ci_dist,
    ymax = fig_dat$rt_diff + fig_dat$ci_dist,
    width = 0.2
  ),
  position = position_dodge(0.9)) + theme_bw() +
  theme(panel.grid.major.x = element_blank())  +
  ylab("Probe-Control RT Difference") +
  xlab("Dataset (individual experimental design)") +
  theme(
    panel.grid.major.y = element_line(color = "#d5d5d5"),
    panel.grid.minor.y = element_line(color = "#d5d5d5"),
    legend.position = "bottom",
    text = element_text(family = "serif", size = 17)
  )


# AUCS

aggr_neat(aucs, values = "aucs", group_by = c("version"), method = 'mean+sd', round_to = 3)
aggr_neat(aucs, values = "accuracies", group_by = c("version"), method = 'mean+sd', round_to = 3)

t_neat(aucs$aucs[aucs$version == 'pred_all'],
       aucs$aucs[aucs$version == 'pred_1'], pair = T, nonparametric = F, round_descr = 3)
t_neat(aucs$accuracies[aucs$version == 'pred_all'],
       aucs$accuracies[aucs$version == 'pred_1'], pair = T, round_descr = 3)

# AUC fig
fig_dat = aucs[aucs$version %in% c("pred_all", "pred_1"), c("version", "dataset", "aucs", "auc_lower", "auc_upper")]
fig_dat$version[fig_dat$version == 'pred_1'] = 'First half'
fig_dat$version[fig_dat$version == 'pred_2'] = 'Second half'
fig_dat$version[fig_dat$version == 'pred_all'] = 'Full test'
fig_dat$dataset = as.factor(fig_dat$dataset)
ggplot2::ggplot(data = fig_dat, aes(x = dataset,
                                    y = aucs,
                                    fill = version)) +
  geom_bar(stat = "identity",
           color = "black",
           position = position_dodge(0.9)) +
  scale_fill_manual(values = c(twocolrs[1], twocolrs[2]),name = NULL) +
  geom_errorbar(aes(
    ymin = fig_dat$auc_lower,
    ymax = fig_dat$auc_upper,
    width = 0.2
  ),
  position = position_dodge(0.9)) + theme_bw() +
  theme(panel.grid.major.x = element_blank())  +
  scale_y_continuous(breaks = c(
    "0" = 0,
    ".25" = 0.25,
    ".50" = 0.5,
    ".75" = 0.75
  )) +
  ylab("Area under the curve") + xlab("Dataset (individual experimental design)") +
  theme(
    panel.grid.major.y = element_line(color = "#d5d5d5"),
    panel.grid.minor.y = element_line(color = "#d5d5d5"),
    legend.position = "bottom",
    text = element_text(family = "serif", size = 17)
  )

# ggpubr::ggarrange(plot_diffs, plot_aucs)

