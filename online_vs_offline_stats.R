library('ggplot2')
library('ggstatsplot')
library('ggpubr')
library('viridis')
library('data.table')
library('trend')
library('tsbox')
library('ppcor')
library('neatStats')

pcorr_ci = function(a, b, c, method = 'pearson') {
    pcor_out = pcor.test(a, b, c, method = method)
    pcor_out = c(pcor_out, ci_from_p(pcor_out$estimate, pcor_out$p.value))
    neatStats:::prnt(paste0(
        'τ = ',
        ro(pcor_out$estimate),
        " [",
        ro(pcor_out$ci_lower),
        ", ",
        ro(pcor_out$ci_upper),
        "]",
        ", p = ",
        ro(pcor_out$p.value, 3)
    ))
}

labels = list(sum = 'Average Sample per Article',
              count = 'Studies per Article',
              mean = 'Average Sample per Study')

oo_data_full = readRDS(neatStats::path_neat('online_vs_offline_data.rds'))
oo_data_full$journal[oo_data_full$journal == 'Psychonomic Bulletin and Review'] = 'Psychonomic Bul. & Rev.'
names(oo_data_full)[names(oo_data_full) == "year"] = "time"

# just to check correct numbers of papers per year
ggplot(oo_data_full, aes(time, fill = journal)) +
    geom_bar() +
    theme_bw()

# set type:
# sum: samples per article
# count: numbers of studies per article
# mean: samples per study (mean study sample per article)
current_type = 'mean' # sum / count / mean

if (current_type == 'mean') {
    labels = Map(function(x) {
        sub('Article', 'Study', x)
    }, labels)
}

oo_data_full$online =  oo_data_full[[paste0('online_', current_type)]]
oo_data_full$offline =  oo_data_full[[paste0('offline_', current_type)]]

# general numbers info
oo_data_full$total = oo_data_full$offline + oo_data_full$online
oo_data_full$ratio = oo_data_full$online / oo_data_full$total
# peek_neat(
#     oo_data,
#     'total',
#     group_by = 'journal',
#     f_plot = plot_neat,
#     binwidth = 5,
#     filt = (total < 500)
# )

# histogramm of offline and online samples
histstyle = list(
    ylab('Count'),
    scale_x_continuous(limits = c(0, 2500)),
    theme(
        #plot.margin = margin(1, 1, 1, 1),
        #plot.title = element_text(hjust = 0.5)
    )
)

offs = oo_data_full$offline[oo_data_full$offline > 0]
ons = oo_data_full$online[oo_data_full$online > 0]
ggpubr::ggarrange(
    plot_neat(offs,
              binwidth = 30,
              parts = c("h", "b")) + histstyle + xlab(sub('Average ', '', labels[[current_type]])) +
        ggtitle(paste0('(A) Offline (N = ', length(offs), ')')),
    plot_neat(ons,
              binwidth = 30,
              parts = c("h", "b")) + histstyle + xlab(sub('Average ', '', labels[[current_type]])) +
        ggtitle(paste0('(B) Online (N = ', length(ons), ')')) +
        theme(axis.title.y = element_text(margin = margin(
            t = 0,
            r = 10,
            b = 0,
            l = 0
        ))),
    ncol = 1
)
# box plots of offline and online samples per journal
peek_style = list(ylab(sub('Average ', '', labels[[current_type]])))
ggpubr::ggarrange(
    peek_neat(oo_data_full[oo_data_full$offline > 0],
              c('offline'), group_by = 'journal') + peek_style + ggtitle('(A) Offline'),
    peek_neat(oo_data_full[oo_data_full$online > 0],
              c('online'), group_by = 'journal',) + peek_style + ggtitle('(B) Online'),
    ncol = 1
)

oo_data_full$type = ifelse(
    oo_data_full$offline == 0,
    'online',
    ifelse(oo_data_full$online == 0, 'offline', 'mixed')
)

oo_data = oo_data_full
max_sample = 2500 # preregistered: 1000; revised: 2500
# check number of outliers
sum(oo_data$online > max_sample) / sum(oo_data$online > 0)
sum(oo_data$offline > max_sample) / sum(oo_data$offline > 0)
(sum(oo_data$online > max_sample) + sum(oo_data$offline > max_sample)) /
    (sum(oo_data$online > 0) + sum(oo_data$offline > 0))
# replace outliers with ceiling
oo_data$online[oo_data$online > max_sample] = max_sample
oo_data$offline[oo_data$offline > max_sample] = max_sample
oo_data$total = oo_data$offline + oo_data$online
oo_data$ratio = oo_data$online / oo_data$total

jnl_data = oo_data[, .(
    total = mean(total),
    online = mean(online),
    offline = mean(offline),
    ratio = mean(ratio)
), by = c('journal', 'time')]

# total samples
jnl_data_total = jnl_data
jnl_data_total$value = jnl_data_total$total
ts_total = tsbox::ts_ts(jnl_data_total[, c('time', 'journal', 'value')])
ts_total[is.na(ts_total)] = 100
mult.mk.test(ts_total, alternative = 'greater')
# mult.mk.test(ts_total)

# online ratio
jnl_data_ratio = jnl_data
jnl_data_ratio$value = jnl_data_ratio$ratio
ts_ratio = tsbox::ts_ts(jnl_data_ratio[, c('time', 'journal', 'value')])
ts_ratio[is.na(ts_ratio)] = 100
mult.mk.test(ts_ratio, alternative = 'greater')
# mult.mk.test(ts_ratio)

# offline alone
jnl_data_offline = jnl_data
jnl_data_offline$value = jnl_data_offline$offline
ts_offline = tsbox::ts_ts(jnl_data_offline[, c('time', 'journal', 'value')])
ts_offline[is.na(ts_offline)] = 100
#mult.mk.test(ts_offline, alternative = 'greater')
mult.mk.test(ts_offline)

##

pcorr_ci(oo_data$total, oo_data$ratio, oo_data$time, method = "kendall")

# citation and authorship
cref_data = oo_data # oo_data[is.na(oo_data$citations), ]

# numbers of papers per year with citation info
# ggplot(cref_data, aes(time, fill = journal)) +
#     geom_bar() +
#     theme_bw()

cref_data$citations = as.numeric(cref_data$citations)
cref_data$authors = as.numeric(cref_data$authors)
pcorr_ci(cref_data$total, cref_data$citations, cref_data$time, method = "kendall")

ggstatsplot::ggscatterstats(
    data = cref_data,
    x = total,
    y = citations,
    results.subtitle = FALSE,
    smooth.line.args = list(alpha = 0, linetype = 0)
)

pcorr_ci(cref_data$ratio, cref_data$citations, cref_data$time, method = "kendall")
pcorr_ci(cref_data$total, cref_data$authors, cref_data$time, method = "kendall")

pcorr_ci(cref_data$ratio, cref_data$authors, cref_data$time, method = "kendall")

pcorr_ci(cref_data$authors,
         cref_data$citations,
         cref_data$time,
         method = "kendall")

ggstatsplot::ggscatterstats(
    data = cref_data,
    x = authors,
    y = citations,
    results.subtitle = FALSE,
    smooth.line.args = list(alpha = 0, linetype = 0)
)


# Figures

# ggstyle = list(stat_summary(geom = "line", fun = mean),
#                # stat_summary(
#                #     geom = "ribbon",
#                #     fun.data = mean_cl_normal,
#                #     alpha = 0.1
#                # ) ,
#                theme_bw())
#
# ggplot(jnl_data_total, aes(time, value)) + ggstyle
#
# ggplot(jnl_data_ratio, aes(time, value)) + ggstyle
#
# ggplot(jnl_data_offline, aes(time, value)) + ggstyle


# Plot
#names(jnl_data)
aggr_data_long = melt(
    jnl_data,
    id.vars = 'time',
    # c('journal', 'time'),
    variable.name = 'type',
    value.name = 'sample',
    measure.vars = c('online', 'offline')
)

aggr_data_long = aggr_data_long[, .(sample = sum(sample) / 5), by = list(time, type)]

virid = 'viridis'
begining = .1
ending = .9
# if (current_type == 'count') {
#     virid = 'mako'
#     begining = .3
#     ending = .7
# }
ggplot(aggr_data_long, aes(x = time, y = sample, fill = type)) +
    geom_area(alpha = 1,
              size = .5,
              colour = "white") +
    scale_fill_viridis(discrete = T,
                       begin = begining,
                       end = ending,
                       option = virid) + # mako / turbo virid
    ylab(labels[[current_type]]) +
    xlab('Year') +
    theme_bw() + theme(
        panel.background = element_rect(fill = NA),
        panel.ontop = TRUE,
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.title = element_blank()
    )#+ggtitle("Experimental Psychology Sample Sizes from 2003 to 2022")

# numbers per halves
mean(aggr_data_long$sample[aggr_data_long$type == 'online' &
                               aggr_data_long$time <= 2012])
mean(aggr_data_long$sample[aggr_data_long$type == 'online' &
                               aggr_data_long$time > 2012])
mean(aggr_data_long$sample[aggr_data_long$type == 'offline' &
                               aggr_data_long$time <= 2012])
mean(aggr_data_long$sample[aggr_data_long$type == 'offline' &
                               aggr_data_long$time > 2012])

# PER JOURNAL
jnl_data_long = melt(
    jnl_data,
    id.vars = c('journal', 'time'),
    variable.name = 'type',
    value.name = 'sample',
    measure.vars = c('online', 'offline')
)

# Plot
ggplot(jnl_data_long, aes(x = time, y = sample, fill = type)) +
    geom_area(alpha = 0.9,
              size = .5,
              colour = "white") +
    scale_fill_viridis(discrete = T,
                       begin = begining,
                       end = ending,
                       option = virid) +
    facet_grid(cols = vars(journal)) +
    ylab(labels[[current_type]]) +
    xlab('Year') +
    theme_bw() + theme(
        panel.background = element_rect(fill = NA),
        panel.ontop = TRUE,
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = 'bold'),
        legend.title = element_blank(),
        legend.position = 'bottom'
    ) #+ ggtitle("Experimental Psychology Sample Sizes from 2003 to 2022")

## RATIOS of sample articles per samples involved

ratios_long = oo_data_full[, .(Ratio = .N / 50), by = list(time,type)]

# imputing zeros for missing types per year
ratios_long = melt(
    dcast(ratios_long, time ~ type, value.var = 'Ratio'),
    measure.vars = c("online", "mixed", "offline"),
    variable.name = "type",
    value.name = "Ratio"
)
ratios_long$Ratio[is.na(ratios_long$Ratio)] = 0

ggplot(ratios_long, aes(x = time, y = Ratio, fill = type)) +
    geom_area(alpha = 1,
              size = .5,
              colour = "white") +
    scale_fill_viridis(discrete = T,
                       begin = .1,
                       end = .9, option = 'cividis') + # cividis / mako / turbo
    ylab('Ratio') +
    xlab('Year') +
    theme_bw() + theme(
        panel.background = element_rect(fill = NA),
        panel.ontop = TRUE,
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.title = element_blank()
    )#+ggtitle("Experimental Psychology Sample Sizes from 2003 to 2022")


# calculations for Sassenberg and Ditrich (2019)

# total
#((203+185)-(113+122))/2

# offline
#((203*(1-0.492)+185*(1-0.505)) - (113*(1-0.090)+122*(1-0.026)))/2

# online
#((203*0.492+185*0.505) - (113*0.090+122*0.026))/2
