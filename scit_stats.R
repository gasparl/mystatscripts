# libs ----

library("neatStats")
library("ggplot2")

# COLLECT DATA ----

setwd(path_neat(''))
main_cit_p <- readRDS("scit_aggr.rds")

main_cit_p = excl_neat(
    main_cit_p,
    overall_er_target < 0.6 &
        overall_er_targetflr < 0.6 &
        overall_er_control < 0.4 &
        overall_er_nontargflr < 0.4,
    group_by = 'exp'
)

table(main_cit_p$rt_MAX_first)

maxes = c('max_first_probe', 'max_first_control4', 'max_first_control2', 'max_first_control3', 'max_first_control1', 'max_last_control2', 'max_last_control3', 'max_last_control1', 'max_last_control4', 'max_last_probe', 'min_of_maxes_first', 'min_of_maxes_last')

# peek_neat(main_cit_p, maxes)

main_cit_p = excl_neat(main_cit_p,
                       min_of_maxes_first >= 10 &
                           min_of_maxes_last >= 10,
                       group_by = 'exp')


# na_cols = sapply(main_cit_p, function(x) sum(ifelse(x == '' | is.na(x) == TRUE | x == 'N/A', 1, 0)))
# names(na_cols[na_cols > 0])



to_excl = c()
to_incl = c()
winner_cols = c()
for (colnam in names(main_cit_p)) {
    if (any(startsWith(
        colnam,
        c(
            "rt_mean_first",
            "rt_mean_last"
        )
    ))) {
        if (any(endsWith(colnam, as.character(11:19)))) {
            to_excl = c(to_excl, colnam)
        } else {
            to_incl = c(to_incl, colnam)
        }
    } else if (startsWith(colnam, 'rt_min_')) {
        winner_cols = c(winner_cols, colnam)
    }
}

main_cit_p$rt_diff_first = main_cit_p$rt_mean_probe_first - main_cit_p$rt_mean_control_first

final_dat = main_cit_p[,!(names(main_cit_p) %in% to_excl)]

final_dat2 = excl_neat(final_dat,
                       rt_diff_first >= 0,
                       group_by = 'exp')
final_dat3 = excl_neat(final_dat,
                       rt_MAX_first == 'probe',
                       group_by = 'exp')


### reshape RTs

to_resh = final_dat[,!(names(final_dat) %in% to_incl)]
# to_resh = final_dat2[,!(names(final_dat2) %in% to_incl)]
# to_resh = final_dat3[,!(names(final_dat3) %in% to_incl)]
dat_long = stats::reshape(
    to_resh,
    direction = 'long',
    varying = winner_cols,
    idvar = 'subject_id',
    timevar = "within_factor",
    v.names = "min_item",
    times = winner_cols
)

dat_long$min_item <-
    factor(dat_long$min_item,
           levels = c('control1', 'control2', 'control3', 'control4', 'probe'))
dat_long$rt_block = do.call(rbind, strsplit(dat_long$within_factor, '_', fixed = TRUE))[,3]
dat_long$rt_number = do.call(rbind, strsplit(dat_long$within_factor, '_', fixed = TRUE))[,4]
dat_long$rt_number = factor(dat_long$rt_number, levels = c(as.character(1:10), 'max'))
dat_plot = dat_long

### plot
library("viridis")
colrs = viridis(5, end = 0.85, direction = -1)

# TODO count percentages numerically

ggplot(dat_plot, aes(rt_number, fill = min_item)) + facet_wrap(vars(rt_block)) +
    geom_bar(aes(y = (..count..) / max(..count..))) + scale_fill_manual(values = colrs) +
    scale_y_continuous(labels = scales::percent) + theme_bw()


### reshape RTs
dat_long2 = stats::reshape(
    final_dat,
    direction = 'long',
    varying = to_incl,
    idvar = 'subject_id',
    timevar = "within_factor",
    v.names = "rt_mean",
    times = to_incl
)
dat_long2$rt_block = do.call(rbind, strsplit(dat_long2$within_factor, '_', fixed = TRUE))[,3]
dat_long2$rt_item = do.call(rbind, strsplit(dat_long2$within_factor, '_', fixed = TRUE))[,4]
dat_long2$rt_number = do.call(rbind, strsplit(dat_long2$within_factor, '_', fixed = TRUE))[,5]
dat_long2$rt_number = factor(dat_long2$rt_number, levels = c(as.character(1:10), 'max'))
dat_plot2 = dat_long2
### plot
ggplot(dat_plot2, aes(x = rt_number, y = rt_mean, group = rt_item, color = rt_item)) + facet_wrap(vars(rt_block)) +
    geom_smooth(method = 'loess')
