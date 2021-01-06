library('neatStats')
library("ggplot2")
library("viridis")

setwd(path_neat(''))


# TODO: add SEX factor
# , facet.by = 'sex'

bass_dems = read.table(
    "age_emo.txt",
    sep = "\t",
    header = TRUE,
    fill = TRUE,
    quote = "\"",
    stringsAsFactors = FALSE
)

for (ages in unique(bass_dems$age_group)) {
    cat('        # ', ages, ' #', fill = TRUE)
    bgroup = bass_dems[bass_dems$age_group == ages,]
    cat('Valence', fill = TRUE)
    corr_neat(bgroup$mean_val_us, bgroup$mean_val_ch)
    cat('Arousal', fill = TRUE)
    corr_neat(bgroup$mean_aro_us, bgroup$mean_aro_ch)
    cat(' ', fill = TRUE)
}

colrs = viridis(5, end = 0.9)

basstyles = list(
    geom_point(size = 0.5) ,
    geom_smooth(method = loess) ,
    scale_color_manual('Age group:', values = colrs) ,
    scale_fill_manual('Age group:', values = colrs) ,
    xlim(1, 9) ,
    ylim(1, 9) ,
    scale_y_continuous(breaks = seq(0, 9)) ,
    scale_x_continuous(breaks = seq(0, 9))  ,
    theme_bw() ,
    theme(
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13),
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 11),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        aspect.ratio = 1
    )
)

ggplot(bass_dems,
       #[bass_dems$age_group !="58+",],
       aes(
           x = mean_val_us,
           y = mean_val_ch,
           color = age_group,
           fill = age_group
       )) + basstyles +
    xlab('Valence US') + ylab('Valence China')


ggpubr::ggscatter(
    bass_dems,#[bass_dems$age_group !="58+",],
    'mean_aro_us',
    'mean_aro_ch',
    add = 'loess',
    conf.int = TRUE,
    color = 'age_group', size = 0.5, palette = colrs,
    xlim = c(1, 9), ylim = c(1, 9),
    legend.title = 'Age group:'
)

ggpubr::ggscatter(
    bass_dems,
    'mean_val_us',
    'mean_aro_us',
    xlab = 'Valence', ylab = 'Arousal',
    add = 'loess', xticks.by =1, yticks.by = 1.0,
    conf.int = TRUE,
    fill = 'age_group',
    color = 'age_group', size = 0.5, palette = colrs,
    xlim = c(1, 9), ylim = c(1, 9),
    legend.title = 'Age group:'
)

ggpubr::ggscatter(
    bass_dems[bass_dems$age_group !="58+",],
    'mean_val_ch',
    'mean_aro_ch',
    xlab = 'Valence', ylab = 'Arousal',
    add = 'loess',
    conf.int = TRUE,
    fill = 'age_group',
    color = 'age_group', size = 0.5, palette = colrs,
    xlim = c(1, 9), ylim = c(1, 9),
    legend.title = 'Age group:'
)


barplot(rep(2,
            5),
        axes = FALSE,
        space = 0,
        col = viridis(5))
