library('ggplot2')
library('ggstatsplot')
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
current_type = 'sum' # sum / count / mean

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
peek_neat(oo_data_full[oo_data_full$offline > 0],
          c('offline'), group_by = 'journal')
peek_neat(oo_data_full[oo_data_full$online > 0],
          c('online'), group_by = 'journal')

oo_data = oo_data_full
max_sample = 2000 # preregistered: 1000
# check number of outliers
sum(oo_data$online > max_sample) / sum(oo_data$online > 0)
sum(oo_data$offline > max_sample) / sum(oo_data$offline > 0)
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


# online ratio
jnl_data_ratio = jnl_data
jnl_data_ratio$value = jnl_data_ratio$ratio
ts_ratio = tsbox::ts_ts(jnl_data_ratio[, c('time', 'journal', 'value')])
ts_ratio[is.na(ts_ratio)] = 100
mult.mk.test(ts_ratio, alternative = 'greater')

# offline alone
jnl_data_offline = jnl_data
jnl_data_offline$value = jnl_data_offline$offline
ts_offline = tsbox::ts_ts(jnl_data_offline[, c('time', 'journal', 'value')])
ts_offline[is.na(ts_offline)] = 100
mult.mk.test(ts_offline, alternative = 'greater')
mult.mk.test(ts_offline)

##

pcorr_ci(oo_data$total, oo_data$ratio, oo_data$time, method = "kendall")
histstyle = list(xlab('Sample'), scale_x_continuous(limits = c(0, 2000)))
#plot_neat(oo_data$total, binwidth = 30) + histstyle
plot_neat(oo_data_full$online[oo_data_full$online > 0], binwidth = 30) + histstyle
plot_neat(oo_data_full$offline, binwidth = 30) + histstyle

# citation and authorship
cref_data = oo_data # oo_data[is.na(oo_data$citations), ]

# numbers of papers per year with citation info
ggplot(cref_data, aes(time, fill = journal)) +
    geom_bar() +
    theme_bw()

cref_data$citations = as.numeric(cref_data$citations)
cref_data$authors = as.numeric(cref_data$authors)
pcorr_ci(cref_data$total, cref_data$citations, cref_data$time)

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

labels = list(sum = 'Average Sample per Article',
              count = 'Average Number of Studies per Article',
              mean = 'Average Sample per Study')
aggr_data_long = aggr_data_long[, .(sample = sum(sample) / 5), by = list(time, type)]

# numbers per halves
mean(aggr_data_long$sample[aggr_data_long$type == 'online' &
                               aggr_data_long$time <= 2012])
mean(aggr_data_long$sample[aggr_data_long$type == 'online' &
                               aggr_data_long$time > 2012])
mean(aggr_data_long$sample[aggr_data_long$type == 'offline' &
                               aggr_data_long$time <= 2012])
mean(aggr_data_long$sample[aggr_data_long$type == 'offline' &
                               aggr_data_long$time > 2012])

ggplot(aggr_data_long, aes(x = time, y = sample, fill = type)) +
    geom_area(alpha = 1,
              size = .5,
              colour = "white") +
    scale_fill_viridis(discrete = T,
                       begin = .1,
                       end = .9) +
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
                       begin = .1,
                       end = .9) +
    facet_grid(cols = vars(journal)) +
    ylab(labels[[current_type]]) +
    xlab('Year') +
    theme_bw() + theme(
        panel.background = element_rect(fill = NA),
        panel.ontop = TRUE,
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.title = element_blank()
    ) #+ ggtitle("Experimental Psychology Sample Sizes from 2003 to 2022")


# calculations for Sassenberg and Ditrich (2019)

# total
#((203+185)-(113+122))/2

# offline
#((203*(1-0.492)+185*(1-0.505)) - (113*(1-0.090)+122*(1-0.026)))/2

# online
#((203*0.492+185*0.505) - (113*0.090+122*0.026))/2
