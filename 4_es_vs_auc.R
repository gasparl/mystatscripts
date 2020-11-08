library('neatStats')
library('bayestestR')
library('ggplot2')
library('sn') # for skewness only

calc_diffs_aucs = function(g_sd_lower,
                           i_sd_lower,
                           g_sd_upper,
                           i_sd_upper,
                           m_g_diff = 0,
                           m_g_avg = 36.78897,
                           limits_x =  c(-100, 150),
                           limits_y = c(0, 0.005),
                           smpl = 10000,
                           samp_rat = 0.5,
                           skewed = FALSE,
                           guilty_gamma = 0,
                           innocent_gamma = 0) {
    m_g_lower =  m_g_avg - m_g_diff / 2
    m_g_upper = m_g_avg + m_g_diff / 2
    
    # smaller g-i difference but smaller SDs:
    g_1 = distribution_normal(n = smpl, sd = g_sd_lower, mean = m_g_lower)
    i_1 = distribution_normal(n = smpl, sd = i_sd_lower, mean = 0)
    
    # larger g-i difference but larger SDs:
    g_2 = distribution_normal(n = smpl, sd = g_sd_upper, mean = m_g_upper)
    i_2 = distribution_normal(n = smpl, sd = i_sd_upper, mean = 0)
    
    if (skewed != FALSE) {
        g_1 = distribution_mixture_normal(
            n = smpl,
            sd = g_sd_lower/2,
            mean = c(m_g_lower*innocent_gamma, m_g_lower*guilty_gamma)
        )
        i_1 = distribution_mixture_normal(
            n = smpl,
            sd = i_sd_lower,
            mean = 0
        )
    }
    
    cat('\n1. Smaller guilty-innocent difference but smaller SDs:',
        fill = T)
    cat(
        '- Guilty mean:',
        mean(g_1),
        ' Guilty SD:',
        sd(g_1),
        ' Innocent SD:',
        sd(i_1),
        fill = T
    )
    t1 = t_neat(
        g_1,
        i_1,
        auc_added = T,
        bf_added = F,
        plots = T,
        x_label = NULL,
        var_names = c('liar', 'truthteller'),
        reverse = F, aspect_ratio = 0.8
    )
    roc1 <<- t1$roc_obj
    fig1 = t1$t_plot +
        scale_x_continuous(limits = limits_x,
                           breaks = seq(-50, 50, by = 50)) + 
        scale_y_continuous(limits = limits_y) + scale_fill_manual(name = NULL, values = c('#006600', '#b3b3ff'))
    
    cat('\n2. Larger guilty-innocent difference but larger SDs:',
        fill = T)
    cat(
        '- Guilty mean:',
        mean(g_2),
        ' Guilty SD:',
        sd(g_2),
        ' Innocent SD:',
        sd(i_2),
        fill = T
    )
    t2 = t_neat(
        g_2,
        i_2,
        auc_added = T,
        bf_added = F,
        plots = T,
        x_label = NULL,
        var_names = c('liar', 'truthteller'),
        reverse = F, aspect_ratio = 0.8
    )
    roc2 <<- t2$roc_obj
    fig2 = t2$t_plot  +
        scale_x_continuous(limits = limits_x,
                           breaks = seq(-50, 50, by = 50)) +
        scale_y_continuous(limits = limits_y) +
        scale_fill_manual(name = NULL, values = c('#006600', '#b3b3ff'))
    
    cat('\nDifferences between two guilty groups:', fill = T)
    t_neat(g_1, g_2, bf_added = F)

    show(ggpubr::annotate_figure(
        ggpubr::ggarrange(
            fig2,
            fig1,
            ncol = 1,
            nrow = 2,
            common.legend = T,
            labels = c('A', 'B'),
            font.label = list(size = 20),
            hjust = -0.9,
            vjust = 1
        ),
        bottom = ggpubr::text_grob(
            "probe-control difference values",
            hjust = 0.4,
            vjust = 0.3,
            size = 16,
            family = 'serif'
        )
    ))
    #fx1 <<- fig1
    #fx2 <<- fig2
    print("---")
    print(round(c(
        t1$stats["d"], t1$stats["auc"], t1$stats["accuracy"]
    ), 3))
    print(round(c(
        t2$stats["d"], t2$stats["auc"], t2$stats["accuracy"]
    ), 3))
}

# 4 between-subject EFFECT SIZE does not mean ACCURACY

# 95% CI:
calc_diffs_aucs(
    g_sd_lower = 33.61352,
    i_sd_lower = 23.50483,
    g_sd_upper = 33.61352,
    i_sd_upper = 23.50483
)

# skew
calc_diffs_aucs(
    g_sd_lower = 60.61352,
    i_sd_lower = 23.50483,
    g_sd_upper = 33.61352,
    i_sd_upper = 23.50483
    , skewed = TRUE, guilty_gamma = 4, innocent_gamma = 0.2
)



